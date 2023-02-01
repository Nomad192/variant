#pragma once

#include <new>
#include <utility>

#include "details.h"

///==================================================================================================================///
/// helper
namespace helper {

template <bool is_trivially_destructible, typename... Types>
struct multi_union_t_DEFAULT_DESTRUCTIBLE {};

template <typename... Types>
using multi_union_t =
    multi_union_t_DEFAULT_DESTRUCTIBLE<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

template <typename... Rest>
struct base_storage_t {};

///==================================================================================================================///
/// multi_union_t_DEFAULT_DESTRUCTIBLE

template <typename First, typename... Rest>
struct multi_union_t_DEFAULT_DESTRUCTIBLE<false, First, Rest...> {
  union {
    First first;
    base_storage_t<Rest...> rest;
    char for_trivial_initialization = 0;
  };

  constexpr explicit multi_union_t_DEFAULT_DESTRUCTIBLE() {}

  template <typename... Args>
  constexpr explicit multi_union_t_DEFAULT_DESTRUCTIBLE(in_place_index_t<0>, Args&&... args)
      : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_DEFAULT_DESTRUCTIBLE(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_DEFAULT_DESTRUCTIBLE(){};
};

template <typename First, typename... Rest>
struct multi_union_t_DEFAULT_DESTRUCTIBLE<true, First, Rest...> {
  union {
    First first;
    base_storage_t<Rest...> rest;
    char for_trivial_initialization = 0;
  };

  constexpr explicit multi_union_t_DEFAULT_DESTRUCTIBLE() {}

  template <typename... Args>
  constexpr explicit multi_union_t_DEFAULT_DESTRUCTIBLE(in_place_index_t<0>, Args&&... args)
      : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_DEFAULT_DESTRUCTIBLE(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_DEFAULT_DESTRUCTIBLE() = default;
};

/// END: multi_union_t_DEFAULT_DESTRUCTIBLE
///==================================================================================================================///
/// base_storage_t

template <typename First, typename... Rest>
struct base_storage_t<First, Rest...> : multi_union_t<First, Rest...> {
  using multi_union_t<First, Rest...>::multi_union_t;

  ///================================================================================================================///
  /// base_set

  constexpr void base_reset(size_t index) {
    if (index == 0)
      this->first.~First();
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.base_reset(index - 1);
  }

  template <size_t Index, typename... Args>
  constexpr void base_constructor(Args&&... args) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      std::construct_at(std::addressof(this->first), std::forward<Args>(args)...);
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.template base_constructor<Index - 1, Args...>(std::forward<Args>(args)...);
  }

  template <size_t Index, typename T>
  constexpr void base_set(T&& other) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      this->first = std::forward<T>(other);
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.template base_set<Index - 1, T>(std::forward<T>(other));
  }

  template <typename Other_MU>
  constexpr void base_constructor_from_other(size_t ind, Other_MU&& from) {
    if (ind == 0 || sizeof...(Rest) == 0)
      std::construct_at(std::addressof(this->first), std::forward<Other_MU>(from).first);
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.template base_constructor_from_other(ind - 1, std::forward<Other_MU>(from).rest);
  }

  template <typename Other_MU>
  constexpr void base_set_from_other(size_t ind, Other_MU&& from) {
    if (ind == 0 || sizeof...(Rest) == 0)
      this->first = std::forward<Other_MU>(from).first;
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.template base_set_from_other(ind - 1, std::forward<Other_MU>(from).rest);
  }

  /// END: base_set
  ///================================================================================================================///
  /// get
  ///------------------------------------------------------///
  /// get &

  template <size_t Index>
  constexpr decltype(auto) get() & {
    if constexpr (Index == 0)
      return (this->first);
    else
      return (this->rest.template get<Index - 1>());
  }

  /// END: get &
  ///------------------------------------------------------///
  /// get &&

  template <size_t Index>
  constexpr decltype(auto) get() && {
    if constexpr (Index == 0)
      return (std::move(this->first));
    else
      return (std::move(this->rest.template get<Index - 1>()));
  }

  /// END: get &&
  ///------------------------------------------------------///
  /// get const&

  template <size_t Index>
  constexpr decltype(auto) get() const& {
    if constexpr (Index == 0)
      return (this->first);
    else
      return (this->rest.template get<Index - 1>());
  }

  /// END: get const&
  ///------------------------------------------------------///
  /// get const&&

  template <size_t Index>
  constexpr decltype(auto) get() const&& {
    if constexpr (Index == 0)
      return (std::move(this->first));
    else
      return (std::move(this->rest.template get<Index - 1>()));
  }

  /// END:: get const&&
  ///------------------------------------------------------///
  /// END: get
  ///================================================================================================================///
};

/// END: base_storage_t
///==================================================================================================================///
/// normal_storage_t

template <typename... Types>
struct normal_storage : base_storage_t<Types...> {
  size_t index = 0;

  constexpr normal_storage() = default;

  template <size_t Index, typename... Args>
  constexpr explicit normal_storage(in_place_index_t<Index>, Args&&... args)
      : index(Index), base_storage_t<Types...>(in_place_index<Index>, std::forward<Args>(args)...) {}

  size_t reset() {
    size_t prev_index = this->index;
    this->index = variant_npos;
    this->base_reset(prev_index);
    return prev_index;
  }

  template <size_t Index, typename... Args>
  constexpr void constructor(Args&&... args) {
    reset();
    this->template base_constructor<Index>(std::forward<Args>(args)...);
    this->index = Index;
  }

  template <size_t Index, typename T>
  constexpr void set(T&& t) {
    if (reset() == Index)
      this->template base_set<Index>(std::forward<T>(t));
    else
      this->template base_constructor<Index>(std::forward<T>(t));
    this->index = Index;
  }

  template <typename Other_PS>
  constexpr void first_constructor_from_other(Other_PS&& other) {
    this->index = variant_npos;
    this->template base_constructor_from_other(other.index, std::forward<Other_PS>(other));
    this->index = other.index;
  }

  template <typename Other_PS>
  constexpr void constructor_from_other(Other_PS&& other) {
    reset();
    this->template base_constructor_from_other(other.index, std::forward<Other_PS>(other));
    this->index = other.index;
  }

  template <typename Other_PS>
  constexpr void set_from_other(Other_PS&& other) {
    if (reset() == other.index)
      this->template base_set_from_other(other.index, std::forward<Other_PS>(other));
    else
      this->template base_constructor_from_other(other.index, std::forward<Other_PS>(other));
    this->index = other.index;
  }
};

/// END: normal_storage_t
///==================================================================================================================///
/// storage_t_DEFAULT_DESTRUCTIBLE

template <bool is_trivially_destructible, typename... Types>
struct storage_t_DEFAULT_DESTRUCTIBLE {};

template <typename... Types>
struct storage_t_DEFAULT_DESTRUCTIBLE<true, Types...> : normal_storage<Types...> {
  using normal_storage<Types...>::normal_storage;

  ~storage_t_DEFAULT_DESTRUCTIBLE() = default;
};

template <typename... Types>
struct storage_t_DEFAULT_DESTRUCTIBLE<false, Types...> : normal_storage<Types...> {
  using normal_storage<Types...>::normal_storage;

  ~storage_t_DEFAULT_DESTRUCTIBLE() {
    this->reset();
  };
};

/// END: storage_t_DEFAULT_DESTRUCTIBLE
///==================================================================================================================///
} // namespace helper

/// END: helper
///==================================================================================================================///
/// storage_t

template <typename... Types>
using storage_t =
    helper::storage_t_DEFAULT_DESTRUCTIBLE<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

/// END: storage_t
///==================================================================================================================///
