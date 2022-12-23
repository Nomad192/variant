#pragma once

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
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename V>
inline constexpr size_t variant_size_v = variant_size<V>::value;

/// END: variant_size
///==================================================================================================================///
/// variant_alternative

template <size_t N, typename V>
struct variant_alternative;

template <size_t N, typename First, typename... Rest>
struct variant_alternative<N, variant<First, Rest...>> : variant_alternative<N - 1, variant<Rest...>> {};

template <typename First, typename... Rest>
struct variant_alternative<0, variant<First, Rest...>> {
  using type = First;
};

///------------------------------------------------------///

template <size_t N, typename V>
using variant_alternative_t = typename variant_alternative<N, V>::type;

///------------------------------------------------------///

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

/// END: variant_alternative
///==================================================================================================================///
/// END: cppreference moment
///==================================================================================================================///
/// Standard Moment

class bad_variant_access : public std::exception {
public:
  bad_variant_access() noexcept = default;
  [[maybe_unused]] explicit bad_variant_access(const char* text_) noexcept : text(text_) {}

  const char* what() const noexcept override {
    return text;
  }

private:
  // Must point to a string with static storage duration:
  const char* text = "bad variant access";
};

/// END: Standard Moment
///==================================================================================================================///
/// My Helper
///==================================================================================================================///
/// get_index_by_type

template <size_t N, typename T, typename... Rest>
struct get_index_by_type_ {
  static constexpr size_t index = -1;
};

template <size_t N, typename T, typename First, typename... Rest>
struct get_index_by_type_<N, T, First, Rest...> {
  static constexpr size_t index = std::is_same_v<T, First> ? N : get_index_by_type_<N + 1, T, Rest...>::index;
};

template <typename T, typename... Types>
using get_index_by_type = get_index_by_type_<0, T, Types...>;

/// END: get_index_by_type
///------------------------------------------------------///
/// get_type_by_construct_type

template <typename T, typename Stored_Type>
concept construct = requires(T&& t)
{
  Stored_Type{std::forward<T>(t)};
};

struct invalid_type{};

template <typename T, typename... Types>
struct get_type_by_construct_type_{
  static constexpr void type() requires(false) { return; }
};

template <typename T, typename First, typename... Rest>
struct get_type_by_construct_type_<T, First, Rest...> : get_type_by_construct_type_<T, Rest...> {
  using get_type_by_construct_type_<T, Rest...>::type;
  static constexpr First type() requires(construct<T, First) { return{}; }
};

template <typename T, typename... Types>
using get_type_by_construct_type = get_type_by_construct_type_<T, Types...>;

/// END: get_type_by_construct_type
///==================================================================================================================///
/// END: My Helper
///==================================================================================================================///
