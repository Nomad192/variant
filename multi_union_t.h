#pragma once

#include <new>
#include <utility>

#include "details.h"

template <bool is_trivially_destructible, typename... Types>
union multi_union_t_ {};

template <typename... Types>
using multi_union_t = multi_union_t_<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

///==================================================================================================================///
/// multi_union_helper_t

template <typename First, typename... Rest>
struct multi_union_helper_t {

  ///================================================================================================================///
  /// set

  template <size_t Index>
  constexpr static void reset(size_t index, multi_union_t<First, Rest...>& mu)
  {
    if (Index == index)
      mu.first.~First();
    else
      return  multi_union_helper_t<Rest...>::template reset<Index + 1>(index, mu.rest);
  }

  template <size_t Index, typename... Args>
  constexpr static void set(size_t index, multi_union_t<First, Rest...>& mu, Args&&... args) {
    multi_union_helper_t<First, Rest...>::template reset<0>(index, mu);
    multi_union_helper_t<First, Rest...>::template only_set<Index>(mu, std::forward<Args>(args)...);
  }

  template <size_t Index, typename... Args>
  constexpr static void only_set(multi_union_t<First, Rest...>& mu, Args&&... args) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      new (std::addressof(mu.first)) First(std::forward<Args>(args)...);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template only_set<Index - 1, Args...>(mu.rest, std::forward<Args>(args)...);
  }

  constexpr static void copy(size_t ind, multi_union_t<First, Rest...> const& from, multi_union_t<First, Rest...>& to) {
    if(ind == 0 || sizeof...(Rest) == 0)
      new (std::addressof(to.first)) First(from.first);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::copy(ind - 1, from.rest, to.rest);
  }

  /// END: set
  ///================================================================================================================///
  /// get
  ///------------------------------------------------------///
  /// get &

  template <size_t Index>
  constexpr static variant_alternative_t<Index, variant<First, Rest...>>& get(multi_union_t<First, Rest...>& mu) {
    if constexpr (Index == 0)
      return mu.first;
    else
      return multi_union_helper_t<Rest...>::template get<Index - 1>(mu.rest);
  }

  /// END: get &
  ///------------------------------------------------------///
  /// get &&

  template <size_t Index>
  constexpr static variant_alternative_t<Index, variant<First, Rest...>>&& get(multi_union_t<First, Rest...>&& mu) {
    if constexpr (Index == 0)
      return std::move(mu.first);
    else
      return multi_union_helper_t<Rest...>::template get<Index - 1>(mu.rest);
  }

  /// END: get &&
  ///------------------------------------------------------///
  /// get const&

  template <size_t Index>
  constexpr static variant_alternative_t<Index, variant<First, Rest...>> const&
  get(multi_union_t<First, Rest...> const& mu) {
    if constexpr (Index == 0)
      return mu.first;
    else
      return multi_union_helper_t<Rest...>::template get<Index - 1>(mu.rest);
  }

  /// END: get const&
  ///------------------------------------------------------///
  /// get const&&

  template <size_t Index>
  constexpr static variant_alternative_t<Index, variant<First, Rest...>> const&&
  get(multi_union_t<First, Rest...> const&& mu) {
    if constexpr (Index == 0)
      return std::move(mu.first);
    else
      return multi_union_helper_t<Rest...>::template get<Index - 1>(mu.rest);
  }

  /// END:: get const&&
  ///------------------------------------------------------///
  /// END: get
  ///================================================================================================================///
};
/// END: multi_union_helper_t
///==================================================================================================================///
/// multi_union_t

template <typename First, typename... Rest_Types>
union multi_union_t_<false, First, Rest_Types...> {
  First first;
  multi_union_t<Rest_Types...> rest;
  char for_trivial_initialization = 0;
  constexpr multi_union_t_() : for_trivial_initialization(0) {}
  //  constexpr multi_union_t() : for_trivial_initialization(0) {}
//
  template <typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_() {}
};

template <typename First, typename... Rest_Types>
union multi_union_t_<true, First, Rest_Types...> {
  First first;
  multi_union_t<Rest_Types...> rest;
  char for_trivial_initialization;
  constexpr multi_union_t_() : for_trivial_initialization(0) {}

  template <typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_() = default;
};

/// END: multi_union_t
///==================================================================================================================///
