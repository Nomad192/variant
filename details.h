#pragma once

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

template <size_t N, typename T, typename... Rest>
struct get_index_by_type_t {
  static const size_t index = N;
};

template <size_t N, typename T, typename First, typename... Rest>
struct get_index_by_type_t<N, T, First, Rest...> {
  static const size_t index = std::is_same_v<T, First> ? N : get_index_by_type_t<N + 1, T, Rest...>::index;
};

template <typename... Types>
using get_index_by_type = get_index_by_type_t<0, Types...>;

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
