#pragma once

#include "variant_imp.h"

///==================================================================================================================///
/// get from Type

template <typename T, typename... Types>
constexpr T& get(variant<Types...>& v) {
  return v.template get_from_type<T>();
}

template <typename T, typename... Types>
constexpr T&& get(variant<Types...>&& v) {
  return v.template get_from_type<T>();
}

template <typename T, typename... Types>
constexpr T const& get(const variant<Types...>& v) {
  return v.template get_from_type<T>();
}

template <typename T, typename... Types>
constexpr T const&& get(const variant<Types...>&& v) {
  return v.template get_from_type<T>();
}

/// END: get from Type
///------------------------------------------------------------------------------------///
/// get from Index

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>>& get(variant<Types...>& v) {
  return v.template get_from_index<Index>();
}

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>>&& get(variant<Types...>&& v) {
  return std::move(v.template get_from_index<Index>());
}

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>> const& get(const variant<Types...>& v) {
  return v.template get_from_index<Index>();
}

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>> const&& get(const variant<Types...>&& v) {
  return std::move(v.template get_from_index<Index>());
}

/// END: get from Index
///==================================================================================================================///
