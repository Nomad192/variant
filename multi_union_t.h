#pragma once

#include <new>
#include <utility>

#include "details.h"

template <typename... Types>
union multi_union_t {};

///==================================================================================================================///
/// multi_union_helper_t

template <typename First, typename... Rest>
struct multi_union_helper_t {

  ///================================================================================================================///
  /// set

  template <size_t Index, typename... Args>
  constexpr static void set(multi_union_t<First, Rest...>& mu, Args&&... args) {
    if constexpr (Index == 0)
      new (std::addressof(mu.first)) First(std::forward<Args>(args)...);
    else
      multi_union_helper_t<Rest...>::template set<Index - 1, Args...>(mu.rest, std::forward<Args>(args)...);
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

template <typename First, typename... Rest_Types>
union multi_union_t<First, Rest_Types...> {
  constexpr multi_union_t() = default;

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  template <typename... Args>
  constexpr explicit multi_union_t(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}
  //
  //  template <size_t N, typename... Args>
  //  constexpr void set(Args&&... args) {
  //    if constexpr (N == 0)
  //      new (std::addressof(first)) First(std::forward<Args>(args)...);
  //    else
  //      rest.template set<N - 1, Args...>(std::forward<Args>(args)...);
  //  }
  //
  //  template <size_t N>
  //  constexpr typename get_type_by_index<N, First, Rest_Types...>::type get() const {
  //    if constexpr (N == 0)
  //      return const_cast<typename get_type_by_index<N, First, Rest_Types...>::type&>(first);
  //    else
  //      return rest.template get<N - 1>();
  //  }

  First first;
  multi_union_t<Rest_Types...> rest;

  ~multi_union_t() = default;
};

///==================================================================================================================///

// template <bool trivial_destructible, typename... Types>
// union Multi_Union_ {};
//
// template <typename... Types>
// using Multi_Union = Multi_Union_<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;
//
/////------------------------------------------------------///
//
// template <typename First, typename... Rest>
// struct storage_t {
//  template <size_t N, typename... Args>
//  constexpr static void set(Multi_Union<First, Rest...>& mu, Args&&... args) {
//    if constexpr (N == 0)
//      new (std::addressof(mu.first)) First(std::forward<Args>(args)...);
//    else
//      storage_t<Rest...>::template set<N - 1, Args...>(mu.rest, std::forward<Args>(args)...);
//  }
//
//  template <size_t N>
//  constexpr static typename get_type_by_index<N, First, Rest...>::type& get(Multi_Union<First, Rest...> const& mu) {
//    if constexpr (N == 0)
//      return const_cast<typename get_type_by_index<N, First, Rest...>::type&>(mu.first);
//    else
//      return storage_t<Rest...>::template get<N - 1>(mu.rest);
//  }
//};
//
/////------------------------------------------------------///
//
// template <typename First, typename... Rest_Types>
// union Multi_Union_<true, First, Rest_Types...> {
//  constexpr Multi_Union_() {}
//
//  First first;
//  Multi_Union<Rest_Types...> rest;
//
//  ~Multi_Union_() = default;
//};
//
// template <typename First, typename... Rest_Types>
// union Multi_Union_<false, First, Rest_Types...> {
//  constexpr Multi_Union_() {}
//
//  First first;
//  Multi_Union<Rest_Types...> rest;
//
//  //  template <size_t N, typename F, typename... RT, typename... Args>
//  //  friend void set(Multi_Union<F, RT...>& mu, Args&&... args);
//
//  ~Multi_Union_(){};
//};

///==================================================================================================================///
