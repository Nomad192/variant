#pragma once

#include "variant_imp.h"

///==================================================================================================================///
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
/// get from Type

template <typename T, typename... Types>
constexpr T& get(variant<Types...>& v) {
  return v.template get_from_index<get_index_by_type<T, Types...>::index>();
}

template <typename T, typename... Types>
constexpr T&& get(variant<Types...>&& v) {
  return v.template get_from_index<get_index_by_type<T, Types...>::index>();
}

template <typename T, typename... Types>
constexpr T const& get(const variant<Types...>& v) {
  return v.template get_from_index<get_index_by_type<T, Types...>::index>();
}

template <typename T, typename... Types>
constexpr T const&& get(const variant<Types...>&& v) {
  return v.template get_from_index<get_index_by_type<T, Types...>::index>();
}

/// END: get from Type
///==================================================================================================================///
/// get_if from Index

template <std::size_t Index, typename... Types>
constexpr std::add_pointer_t<variant_alternative_t<Index, variant<Types...>>>
get_if(variant<Types...>* pv) noexcept
{
  return pv->template get_if_from_index<Index>();
}

template <std::size_t Index, typename... Types>
constexpr std::add_pointer_t<const variant_alternative_t<Index, variant<Types...>>>
get_if(const variant<Types...>* pv) noexcept
{
  return pv->template get_if_from_index<Index>();
}

/// END: get_if from Index
///==================================================================================================================///
/// get_if from Type

template <class T, typename... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept
{
  return pv->template get_if_from_index<get_index_by_type<T, Types...>::index>();
}

template <class T, typename... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept
{
  return pv->template get_if_from_index<get_index_by_type<T, Types...>::index>();
}

/// END: get_if from Type
///==================================================================================================================///
