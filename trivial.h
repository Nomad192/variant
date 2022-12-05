#pragma once

#include <new>
#include <utility>

template <typename First, typename... Rest>
class variant;

///==================================================================================================================///
/// cppreference moment:

template <class T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};
template <class T>
inline constexpr in_place_type_t<T> in_place_type{};

///------------------------------------------------------///

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};
template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

///------------------------------------------------------///

template <typename V>
struct variant_size;

template <typename V>
struct variant_size<const V> : variant_size<V> {};

template <typename V>
struct variant_size<volatile V> : variant_size<V> {};

template <typename V>
struct variant_size<const volatile V> : variant_size<V> {};

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename V>
inline constexpr size_t variant_size_v = variant_size<V>::value;

///------------------------------------------------------///

template <size_t N, typename V>
struct variant_alternative;

template <size_t N, typename First, typename... Rest>
struct variant_alternative<N, variant<First, Rest...>> : variant_alternative<N - 1, variant<Rest...>> {};

template <typename First, typename... Rest>
struct variant_alternative<0, variant<First, Rest...>> {
  using type = First;
};

template <size_t N, typename V>
using variant_alternative_t = typename variant_alternative<N, V>::type;

template <size_t N, typename V>
struct variant_alternative<N, const V> {
  using type = std::add_const_t<variant_alternative_t<N, V>>;
};

template <size_t N, typename V>
struct variant_alternative<N, volatile V> {
  using type = std::add_volatile_t<variant_alternative_t<N, V>>;
};

template <size_t N, typename V>
struct variant_alternative<N, const volatile V> {
  using type = std::add_cv_t<variant_alternative_t<N, V>>;
};

///------------------------------------------------------///


///==================================================================================================================///

template <bool trivial_destructible, typename... Types>
union Multi_Union_ {};

template <typename... Types>
using Multi_Union = Multi_Union_<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

///------------------------------------------------------///

template <typename First, typename... Rest>
struct set_t {
  template <size_t N, typename... Args>
  constexpr static void set(Multi_Union<First, Rest...>& mu, Args&&... args) {
    if constexpr (N == 0)
      new (&mu.first) First(std::forward<Args>(args)...);
    else
      set_t<Rest...>::template set<N - 1, Args...>(mu.rest, std::forward<Args>(args)...);
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
  constexpr base() {}
  template <size_t N, typename... Args,
            typename Valid_Index =
                std::enable_if_t<N<sizeof...(Rest_Types) + 1>,
                                 typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
                                     typename get_type_by_index<N, First, Rest_Types...>::type,
                                     Args...>>> constexpr base(in_place_index_t<N>, Args&&... args) : current_index(N) {
    set_t<First, Rest_Types...>::template set<N, Args...>(storage, std::forward<Args>(args)...);
  }

  template <
      typename T, typename... Args,
      typename Valid_Type = std::enable_if_t<
          get_index_by_type<0, T, First, Rest_Types...>::value<sizeof...(Rest_Types) + 1>,
          typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
              T, Args...>>> constexpr base(in_place_type_t<T>,
                                           Args&&... args) : current_index(get_index_by_type<0, T, First,
                                                                                             Rest_Types...>::value) {
    set_t<First, Rest_Types...>::template set<get_index_by_type<0, T, First, Rest_Types...>::value, Args...>(
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
