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

  constexpr static void reset(size_t index, multi_union_t<First, Rest...>& mu) {
    if (index == 0)
      mu.first.~First();
    else if constexpr (sizeof...(Rest) > 0)
      return multi_union_helper_t<Rest...>::reset(index - 1, mu.rest);
  }

  template <size_t Index, typename... Args>
  constexpr static void constructor(multi_union_t<First, Rest...>& mu, Args&&... args) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      new (std::remove_const_t<void*>(std::addressof(mu.first))) First(std::forward<Args>(args)...);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template constructor<Index - 1, Args...>(mu.rest, std::forward<Args>(args)...);
  }

  template <size_t Index, typename T>
  constexpr static void set(multi_union_t<First, Rest...>& mu, T&& other) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      mu.first = std::forward<T>(other);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template set<Index - 1, T>(mu.rest, std::forward<T>(other));
  }

  template <typename Other_MU>
  constexpr static void constructor_from_other(size_t ind, Other_MU&& from, multi_union_t<First, Rest...>& to) {
    if (ind == 0 || sizeof...(Rest) == 0)
      new (std::remove_const_t<void*>(std::addressof(to.first))) First(std::forward<Other_MU>(from).first);
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template constructor_from_other(ind - 1, std::forward<Other_MU>(from).rest,
                                                                     to.rest);
  }

  template <typename Other_MU>
  constexpr static void set_from_other(size_t ind, Other_MU&& from, multi_union_t<First, Rest...>& to) {
    if (ind == 0 || sizeof...(Rest) == 0)
      to.first = std::forward<Other_MU>(from).first;
    else if constexpr (sizeof...(Rest) > 0)
      multi_union_helper_t<Rest...>::template set_from_other(ind - 1, std::forward<Other_MU>(from).rest, to.rest);
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

template <typename First, typename... Rest>
union multi_union_t_<false, First, Rest...> {
  First first;
  multi_union_t<Rest...> rest;
  char for_trivial_initialization;
  constexpr multi_union_t_() : for_trivial_initialization(0) {}
  //  constexpr multi_union_t() : for_trivial_initialization(0) {}

  template <typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_(){};
};

template <typename First, typename... Rest>
union multi_union_t_<true, First, Rest...> {
  First first;
  multi_union_t<Rest...> rest;
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

template <typename... Types>
struct base_storage {
  size_t index = 0;
  multi_union_t<Types...> value;
  using mu_help = multi_union_helper_t<Types...>;

  constexpr base_storage() = default;

  size_t reset() {
    size_t prev_index = index;
    index = variant_npos;
    mu_help::reset(prev_index, value);
    return prev_index;
  }

  template <size_t Index, typename... Args>
  constexpr explicit base_storage(in_place_index_t<Index>, Args&&... args)
      : index(Index), value(in_place_index<Index>, std::forward<Args>(args)...) {}

  template <size_t Index, typename... Args>
  constexpr void constructor(Args&&... args) {
    reset();
    mu_help::template constructor<Index>(value, std::forward<Args>(args)...);
    index = Index;
  }

  template <size_t Index, typename T>
  constexpr void set(T&& t) {
    if (reset() == Index)
      mu_help::template set<Index>(value, std::forward<T>(t));
    else
      mu_help::template constructor<Index>(value, std::forward<T>(t));
    index = Index;
  }

  template <typename Other_PS>
  constexpr void constructor_from_other(Other_PS&& other) {
    size_t new_index = other.index;
    reset();
    mu_help::template constructor_from_other(other.index, std::forward<Other_PS>(other).value, this->value);
    index = new_index;
  }

  template <typename Other_PS>
  constexpr void set_from_other(Other_PS&& other) {
    size_t new_index = other.index;
    if (reset() == other.index)
      mu_help::template set_from_other(other.index, std::forward<Other_PS>(other).value, this->value);
    else
      mu_help::template constructor_from_other(other.index, std::forward<Other_PS>(other).value, this->value);
    index = new_index;
  }
};

namespace helper {
template <bool is_trivially_destructible, typename... Types>
struct storage_t_DONT_USE {};

template <typename... Types>
struct storage_t_DONT_USE<true, Types...> : base_storage<Types...> {
  using base_storage<Types...>::base_storage;

  ~storage_t_DONT_USE() = default;
};

template <typename... Types>
struct storage_t_DONT_USE<false, Types...> : base_storage<Types...> {
  using base_storage<Types...>::base_storage;

  ~storage_t_DONT_USE() {
    multi_union_helper_t<Types...>::reset(this->index, this->value);
  };
};
} // namespace helper

template <typename... Types>
using storage_t = helper::storage_t_DONT_USE<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

/// END: storage_t
///==================================================================================================================///
