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
  constexpr static void reset(size_t index, multi_union_t<First, Rest...>& mu) {
    if (Index == index)
      mu.first.~First();
    else if constexpr (sizeof...(Rest) > 0)
      return multi_union_helper_t<Rest...>::template reset<Index + 1>(index, mu.rest);
  }

  template <size_t Index, typename... Args>
  constexpr static void set(size_t index, multi_union_t<First, Rest...>& mu, Args&&... args) {
    multi_union_helper_t<First, Rest...>::template reset<0>(index, mu);
    multi_union_helper_t<First, Rest...>::template only_set<Index>(mu, std::forward<Args>(args)...);
  }

  template <size_t Index, typename T>
  constexpr static void operator_set(size_t index, multi_union_t<First, Rest...>& mu, T&& other) {
    multi_union_helper_t<First, Rest...>::template reset<0>(index, mu);
    multi_union_helper_t<First, Rest...>::template only_operator_set<Index>(mu, std::forward<T>(other));
  }

  template <size_t Index, typename... Args>
  constexpr static void only_set(multi_union_t<First, Rest...>& mu, Args&&... args) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      new (std::remove_const_t<void*>(std::addressof(mu.first))) First(std::forward<Args>(args)...);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template only_set<Index - 1, Args...>(mu.rest, std::forward<Args>(args)...);
  }

  template <size_t Index, typename T>
  constexpr static void only_operator_set(multi_union_t<First, Rest...>& mu, T&& other) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      mu.first = std::forward<T>(other);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template only_operator_set<Index - 1, T>(mu.rest, std::forward<T>(other));
  }

  template <typename Other_Variant>
  constexpr static void construct_from_other(size_t ind, Other_Variant&& from, multi_union_t<First, Rest...>& to) {
    if (ind == 0 || sizeof...(Rest) == 0)
      new (std::remove_const_t<void*>(std::addressof(to.first))) First(std::forward<Other_Variant>(from).first);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template construct_from_other(ind - 1, from.rest, to.rest);
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
  char for_trivial_initialization;
  constexpr multi_union_t_() : for_trivial_initialization(0) {}
  //  constexpr multi_union_t() : for_trivial_initialization(0) {}

  template <typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_() {};
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
/// storage_t

template <bool is_trivially_destructible, typename... Types>
struct storage_t_ {};

template <typename... Types>
using storage_t = storage_t_<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

template <typename... Types>
struct storage_t_<true, Types...> {
  size_t index = 0;
  multi_union_t<Types...> value;

  constexpr storage_t_() = default;

  template <size_t N, typename... Args>
  constexpr explicit storage_t_(in_place_index_t<N>, Args&&... args)
      : index(N), value(in_place_index<N>, std::forward<Args>(args)...) {}

  ~storage_t_() = default;
};

template <typename... Types>
struct storage_t_<false, Types...> {
  size_t index = 0;
  multi_union_t<Types...> value;

  constexpr storage_t_() = default;

  template <size_t N, typename... Args>
  constexpr explicit storage_t_(in_place_index_t<N>, Args&&... args)
      : index(N), value(in_place_index<N>, std::forward<Args>(args)...) {}

  ~storage_t_() {
    multi_union_helper_t<Types...>::template reset<0>(index, value);
  };
};

/// END: storage_t
///==================================================================================================================///
