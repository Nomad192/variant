#pragma once

#include <new>
#include <utility>

#include "details.h"
#include "Multi_Union.h"

///==================================================================================================================///

template <typename First, typename... Rest_Types>
struct base {
  constexpr base() {
    storage_t<First, Rest_Types...>::template set<0>(storage);
  }
  template <size_t N, typename... Args,
            typename Valid_Index =
                std::enable_if_t<N<sizeof...(Rest_Types) + 1>,
                                 typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
                                     typename get_type_by_index<N, First, Rest_Types...>::type,
                                     Args...>>> constexpr base(in_place_index_t<N>, Args&&... args) : current_index(N) {
    storage_t<First, Rest_Types...>::template set<N, Args...>(storage, std::forward<Args>(args)...);
  }

  template <
      typename T, typename... Args,
      typename Valid_Type = std::enable_if_t<
          get_index_by_type<T, First, Rest_Types...>::index<sizeof...(Rest_Types) + 1>,
          typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
              T, Args...>>> constexpr base(in_place_type_t<T>,
                                           Args&&... args) : current_index(get_index_by_type<T, First,
                                                                                             Rest_Types...>::index) {
    storage_t<First, Rest_Types...>::template set<get_index_by_type<T, First, Rest_Types...>::index, Args...>(
        storage, std::forward<Args>(args)...);
  }

  template <typename T> constexpr base(T x) : base(in_place_type<T>, x) {}

protected:
  size_t current_index = 0;
  Multi_Union<First, Rest_Types...> storage;
};

///==================================================================================================================///

template <bool trivial = false, typename... Types>
struct trivial_copy_constructable_base_ : base<Types...> {
  using base<Types...>::base;
  constexpr trivial_copy_constructable_base_(const trivial_copy_constructable_base_& other) : base<Types...>() { ///???
    ///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  }
  constexpr trivial_copy_constructable_base_(trivial_copy_constructable_base_&&) = default;
};

template <typename... Types>
struct trivial_copy_constructable_base_<true, Types...> : base<Types...> {
  using base<Types...>::base;
};

template <typename... Types>
using trivial_copy_constructable_base =
    trivial_copy_constructable_base_<std::conjunction_v<std::is_trivially_copy_constructible<Types>...>, Types...>;

///==================================================================================================================///

template <bool trivial = false, typename... Types>
struct trivial_copy_assign_base_ : trivial_copy_constructable_base<Types...> {
  using trivial_copy_constructable_base<Types...>::trivial_copy_constructable_base;
  constexpr trivial_copy_assign_base_(const trivial_copy_assign_base_& other) = default;
  trivial_copy_assign_base_& operator=(const trivial_copy_assign_base_& other) {
    ///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    return *this;
  }
};

template <typename... Types>
struct trivial_copy_assign_base_<true, Types...> : trivial_copy_constructable_base<Types...> {
  using trivial_copy_constructable_base<Types...>::trivial_copy_constructable_base;
};

template <typename... Types>
using trivial_copy_assign_base =
    trivial_copy_assign_base_<std::conjunction_v<std::is_trivially_copy_constructible<Types>...> &&
                                  std::conjunction_v<std::is_trivially_copy_assignable<Types>...>,
                              Types...>;

///==================================================================================================================///

template <bool trivial = false, typename... Types>
struct trivial_move_constructable_base_ : trivial_copy_assign_base<Types...> {
  using trivial_copy_assign_base<Types...>::trivial_copy_assign_base;
  constexpr trivial_move_constructable_base_(const trivial_move_constructable_base_&) = default;
  constexpr trivial_move_constructable_base_(trivial_move_constructable_base_&& other) {
    ///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  }
  trivial_move_constructable_base_& operator=(const trivial_move_constructable_base_&) = default;
};

template <typename... Types>
struct trivial_move_constructable_base_<true, Types...> : trivial_copy_assign_base<Types...> {
  using trivial_copy_assign_base<Types...>::trivial_copy_assign_base;
};

template <typename... Types>
using trivial_move_constructable_base =
    trivial_move_constructable_base_<std::conjunction_v<std::is_trivially_move_constructible<Types>...>, Types...>;

///==================================================================================================================///

template <bool trivial = false, typename... Types>
struct trivial_move_assign_base_ : trivial_move_constructable_base<Types...> {
  using trivial_move_constructable_base<Types...>::trivial_move_constructable_base;
  constexpr trivial_move_assign_base_(const trivial_move_assign_base_&) = default;
  constexpr trivial_move_assign_base_(trivial_move_assign_base_&& other) = default;

  trivial_move_assign_base_& operator=(const trivial_move_assign_base_&) = default;
  trivial_move_assign_base_& operator=(trivial_move_assign_base_&& other) {
    ///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    return *this;
  }
};

template <typename... Types>
struct trivial_move_assign_base_<true, Types...> : trivial_move_constructable_base<Types...> {
  using trivial_move_constructable_base<Types...>::trivial_move_constructable_base;
};

template <typename... Types>
using trivial_move_assign_base =
    trivial_move_assign_base_<std::conjunction_v<std::is_trivially_move_constructible<Types>...> &&
                                  std::conjunction_v<std::is_trivially_move_assignable<Types>...>,
                              Types...>;

///==================================================================================================================///
