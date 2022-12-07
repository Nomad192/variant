#pragma once

#include <new>
#include <utility>

#include "details.h"

///==================================================================================================================///

template <bool trivial_destructible, typename... Types>
union Multi_Union_ {};

template <typename... Types>
using Multi_Union = Multi_Union_<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

///------------------------------------------------------///

template <typename First, typename... Rest>
struct storage_t {
  template <size_t N, typename... Args>
  constexpr static void set(Multi_Union<First, Rest...>& mu, Args&&... args) {
    if constexpr (N == 0)
      new (std::addressof(mu.first)) First(std::forward<Args>(args)...);
    else
      storage_t<Rest...>::template set<N - 1, Args...>(mu.rest, std::forward<Args>(args)...);
  }

  template <size_t N>
  constexpr static typename get_type_by_index<N, First, Rest...>::type& get(Multi_Union<First, Rest...> const& mu) {
    if constexpr (N == 0)
      return const_cast<typename get_type_by_index<N, First, Rest...>::type&>(mu.first);
    else
      return storage_t<Rest...>::template get<N - 1>(mu.rest);
  }
};

///------------------------------------------------------///

template <typename First, typename... Rest_Types>
union Multi_Union_<true, First, Rest_Types...> {
  constexpr Multi_Union_() {}

  First first;
  Multi_Union<Rest_Types...> rest;

  ~Multi_Union_() = default;
};

template <typename First, typename... Rest_Types>
union Multi_Union_<false, First, Rest_Types...> {
  constexpr Multi_Union_() {}

  First first;
  Multi_Union<Rest_Types...> rest;

  //  template <size_t N, typename F, typename... RT, typename... Args>
  //  friend void set(Multi_Union<F, RT...>& mu, Args&&... args);

  ~Multi_Union_(){};
};

///==================================================================================================================///
