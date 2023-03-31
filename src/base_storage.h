#pragma once

#include <memory>
#include <new>
#include <utility>

#include "details.h"

///==================================================================================================================///
/// helper
namespace helper {

template <bool is_trivially_destructible, typename... Types>
struct multi_union_t_default_destructible {};

template <typename... Types>
using multi_union_t =
    multi_union_t_default_destructible<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

template <typename... Rest>
struct base_storage_t {};

///==================================================================================================================///
/// multi_union_t_default_destructible

template <typename First, typename... Rest>
struct multi_union_t_default_destructible<false, First, Rest...> {
  union {
    First first;
    base_storage_t<Rest...> rest;
  };

  constexpr explicit multi_union_t_default_destructible() {}

  template <typename... Args>
  constexpr explicit multi_union_t_default_destructible(in_place_index_t<0>, Args&&... args)
      : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_default_destructible(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_default_destructible(){};
};

template <typename First, typename... Rest>
struct multi_union_t_default_destructible<true, First, Rest...> {
  union {
    First first;
    base_storage_t<Rest...> rest;
  };

  constexpr explicit multi_union_t_default_destructible() {}

  template <typename... Args>
  constexpr explicit multi_union_t_default_destructible(in_place_index_t<0>, Args&&... args)
      : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr explicit multi_union_t_default_destructible(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  ~multi_union_t_default_destructible() = default;
};

/// END: multi_union_t_default_destructible
///==================================================================================================================///
/// base_storage_t

template <typename First, typename... Rest>
struct base_storage_t<First, Rest...> : multi_union_t<First, Rest...> {
  using multi_union_t<First, Rest...>::multi_union_t;

  ///================================================================================================================///
  /// base_set

  template <size_t Index>
  constexpr void base_reset() {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      this->first.~First();
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.template base_reset<Index - 1>();
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

  template <size_t Index, typename Other_MU>
  constexpr void base_constructor_from_other(Other_MU&& from) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      std::construct_at(std::addressof(this->first), std::forward<Other_MU>(from).first);
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.template base_constructor_from_other<Index - 1>(std::forward<Other_MU>(from).rest);
  }

  template <size_t Index, typename Other_MU>
  constexpr void base_set_from_other(Other_MU&& from) {
    if constexpr (sizeof...(Rest) == 0 || Index == 0)
      this->first = std::forward<Other_MU>(from).first;
    else if constexpr (sizeof...(Rest) > 0)
      this->rest.template base_set_from_other<Index - 1>(std::forward<Other_MU>(from).rest);
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
} // namespace helper
