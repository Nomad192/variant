#pragma once

#include <exception>
#include <utility>

template <typename First, typename... Rest>
struct variant;

///==================================================================================================================///
/// https://cppreference.com Moment

inline constexpr std::size_t variant_npos = -1;

///==================================================================================================================///
/// in_place

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

/// END: in_place
///==================================================================================================================///
/// variant_size

template <typename V>
struct variant_size;

template <typename V>
struct variant_size<const V> : variant_size<V> {};

template <typename V>
struct variant_size<volatile V> : variant_size<V> {};

template <typename V>
struct variant_size<const volatile V> : variant_size<V> {};

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename V>
inline constexpr std::size_t variant_size_v = variant_size<V>::value;

/// END: variant_size
///==================================================================================================================///
/// variant_alternative

template <std::size_t N, typename V>
struct variant_alternative;

template <std::size_t N, typename First, typename... Rest>
struct variant_alternative<N, variant<First, Rest...>> : variant_alternative<N - 1, variant<Rest...>> {};

template <typename First, typename... Rest>
struct variant_alternative<0, variant<First, Rest...>> {
  using type = First;
};

///------------------------------------------------------///

template <std::size_t N, typename V>
using variant_alternative_t = typename variant_alternative<N, V>::type;

///------------------------------------------------------///

template <std::size_t N, typename V>
struct variant_alternative<N, const V> {
  using type = std::add_const_t<variant_alternative_t<N, V>>;
};

template <std::size_t N, typename V>
struct variant_alternative<N, volatile V> {
  using type = std::add_volatile_t<variant_alternative_t<N, V>>;
};

template <std::size_t N, typename V>
struct variant_alternative<N, const volatile V> {
  using type = std::add_cv_t<variant_alternative_t<N, V>>;
};

/// END: variant_alternative
///==================================================================================================================///
/// END: https://cppreference.com Moment
///==================================================================================================================///
/// Standard Moment

class bad_variant_access : public std::exception {
public:
  bad_variant_access() noexcept; // = default;

  const char* what() const noexcept override;
};

/// END: Standard Moment
///==================================================================================================================///
/// Helper
///==================================================================================================================///
/// get_index_by_type

namespace helper {
template <std::size_t N, typename T, typename... Rest>
struct get_index_by_type_impl {
  static constexpr std::size_t index = -1;
};

template <std::size_t N, typename T, typename First, typename... Rest>
struct get_index_by_type_impl<N, T, First, Rest...> {
  static constexpr std::size_t index = std::is_same_v<T, First> ? N : get_index_by_type_impl<N + 1, T, Rest...>::index;
};
} // namespace helper

template <typename T, typename... Types>
using get_index_by_type = helper::get_index_by_type_impl<0, T, Types...>;

/// END: get_index_by_type
///------------------------------------------------------///
/// get_type_by_construct_type

namespace helper {
template <typename T, typename Stored_Type>
concept construct = requires(T && t) {
  Stored_Type{std::forward<T>(t)};
};

template <typename T, typename... Types>
struct get_type_by_construct_type_impl {
  static constexpr void type() requires(true) {
    return;
  };
};

template <typename T, typename First, typename... Rest>
struct get_type_by_construct_type_impl<T, First, Rest...> : get_type_by_construct_type_impl<T, Rest...> {
  using get_type_by_construct_type_impl<T, Rest...>::type;
  static constexpr First type(First const& obj) requires(construct<T, First[]>) {
    return {};
  }
};
} // namespace helper

template <typename T, typename... Types>
using get_type_by_construct_type =
    decltype(helper::get_type_by_construct_type_impl<T, Types...>::type(std::declval<T>()));

/// END: get_type_by_construct_type
///==================================================================================================================///
/// END: Helper
///==================================================================================================================///
