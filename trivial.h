#pragma once

#include <new>
#include <utility>

///==================================================================================================================///

template <class T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};
template <class T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};
template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

///==================================================================================================================///

template <bool trivial_destructible, typename... Types>
union Multi_Union_ {};

template <typename... Types>
using Multi_Union = Multi_Union_<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

template <typename First, typename... Rest_Types>
union Multi_Union_<true, First, Rest_Types...> {
  Multi_Union_(){}

  First first;
  Multi_Union<Rest_Types...> rest;

  template <typename... Args>
  constexpr Multi_Union_(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr Multi_Union_(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  void set(Args&&... args) {
    if constexpr (N == 0)
      new (&first) First(std::forward<Args>(args)...);
    else
      rest.template set<N - 1>(args...);
  }

  ~Multi_Union_() = default;
};

template <typename First, typename... Rest_Types>
union Multi_Union_<false, First, Rest_Types...> {
  Multi_Union_(){}

  First first;
  Multi_Union<Rest_Types...> rest;

  template <typename... Args>
  constexpr Multi_Union_(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr Multi_Union_(in_place_index_t<N>, Args&&... args)
      : rest(in_place_index<N - 1>, std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  void set(Args&&... args) {
    if constexpr (N == 0)
      new (&first) First(std::forward<Args>(args)...);
    else
      rest.template set<N - 1>(args...);
  }

  ~Multi_Union_(){};
};

///==================================================================================================================///

template <size_t N, typename T, typename... Rest>
struct get_index_by_type {
  static const size_t value = N;
};

template <size_t N, typename T, typename First, typename... Rest>
struct get_index_by_type<N, T, First, Rest...> {
  static const size_t value = std::is_same_v<T, First> ? N : get_index_by_type<N + 1, T, Rest...>::value;
};

///------------------------------------------------------///

template <size_t, typename...>
struct get_type_by_index;

template <typename First, typename... Rest>
struct get_type_by_index<0, First, Rest...> {
  using type = First;
};

template <size_t N, typename First, typename... Rest>
struct get_type_by_index<N, First, Rest...> {
  using type = typename get_type_by_index<N - 1, Rest...>::type;
};

///==================================================================================================================///

template <typename First, typename... Rest_Types>
struct base {
protected:
  size_t current_index = 0;
  Multi_Union<First, Rest_Types...> storage;

public:
  base() {}

  //  constexpr base(in_place_type_t) {
  //    static_assert(current_index < (sizeof...(Rest_Types) + 1));
  //
  //  }

  template <size_t N, typename... Args>
  constexpr base(in_place_index_t<N>, Args&&... args)
      : current_index(N), storage(in_place_index<N>, std::forward<Args>(args)...) {}

  template <typename T>
  constexpr base(T x) : current_index(get_index_by_type<0, T, First, Rest_Types...>::value) {
    // static_assert(current_index < (sizeof...(Rest_Types) + 1));
    storage.template set<get_index_by_type<0, T, First, Rest_Types...>::value, T>(std::move(x));
  }
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
