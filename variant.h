#pragma once

#include "enable.h"
#include "trivial.h"

template <typename First, typename... Rest_Types>
struct variant : trivial_move_assign_base<First, Rest_Types...>,
                 default_constructable_first<First>,
                 nothrow_default_constructable_first<First>,
                 nothrow_move_construct_base<First, Rest_Types...>,
                 //nothrow_move_assign_base<First, Rest_Types...>,
                 copy_assign_base<First, Rest_Types...>,
                 move_assign_base<First, Rest_Types...>,
                 copy_construct_base<First, Rest_Types...>,
                 move_construct_base<First, Rest_Types...> {

  using trivial_move_assign_base<First, Rest_Types...>::trivial_move_assign_base;
  //  constexpr variant(nullopt_t) : variant(){};
  //  constexpr explicit variant(T x)
  //      : trivial_move_assign_base<Types>(in_place, std::move(x)){};
  constexpr variant() noexcept(std::is_nothrow_constructible_v<First>) = default;
  constexpr variant(const variant&) = default;
  constexpr variant(variant&&) noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Rest_Types>...,
                                                           std::is_nothrow_move_constructible<First>>) = default;
  variant& operator=(const variant&) = default;
  variant& operator=(variant&&) noexcept(
      std::conjunction_v<std::is_nothrow_move_assignable<Rest_Types>..., std::is_nothrow_move_assignable<First>>&&
          std::conjunction_v<std::is_nothrow_move_constructible<Rest_Types>...,
                             std::is_nothrow_move_constructible<First>>) = default;

  //  constexpr T& operator*() noexcept {
  //    return this->data;
  //  }
  //  constexpr T const& operator*() const noexcept {
  //    return this->data;
  //  }
  //
  //  constexpr T* operator->() noexcept {
  //    return &this->data;
  //  }
  //  constexpr T const* operator->() const noexcept {
  //    return &this->data;
  //  }
  //
  //  [[nodiscard]] explicit constexpr operator bool() const noexcept {
  //    return this->has_value();
  //  }
  //
  //  variant& operator=(nullopt_t) noexcept {
  //    this->reset();
  //    return *this;
  //  }

  //  template <typename... Args>
  //  constexpr void emplace(Args&&... args) {
  //    this->reset();
  //    new (&this->data) T(std::forward<Args>(args)...);
  //    this->is_present = true;
  //  }

  //  [[nodiscard]] constexpr bool has_value() const noexcept {
  //    return this->is_present;
  //  }
  //
  //  void swap(variant& other) {
  //    if (has_value() && other.has_value()) {
  //      using std::swap;
  //      swap(this->data, other.data);
  //    } else if (has_value() && !other.has_value()) {
  //      other = std::move(*this);
  //      this->reset();
  //    } else if (!has_value() && other.has_value()) {
  //      other.swap2(*this);
  //    }
  //  }
};

template <typename... Types>
constexpr bool operator==(variant<Types...> const& a, variant<Types...> const& b) {
  if (bool(a) != bool(b))
    return false;

  if (!bool(a) || !bool(b))
    return true;

  return *a == *b;
}

template <typename... Types>
constexpr bool operator!=(variant<Types...> const& a, variant<Types...> const& b) {
  if (bool(a) != bool(b))
    return true;

  if (!bool(a) || !bool(b))
    return false;

  return *a != *b;
}

template <typename... Types>
constexpr bool operator<(variant<Types...> const& a, variant<Types...> const& b) {
  if (!bool(b))
    return false;

  if (!bool(a))
    return true;

  return *a < *b;
}

template <typename... Types>
constexpr bool operator<=(variant<Types...> const& a, variant<Types...> const& b) {
  if (!bool(a))
    return true;

  if (!bool(b))
    return false;

  return *a <= *b;
}

template <typename... Types>
constexpr bool operator>(variant<Types...> const& a, variant<Types...> const& b) {
  if (!bool(a))
    return false;

  if (!bool(b))
    return true;

  return *a > *b;
}

template <typename... Types>
constexpr bool operator>=(variant<Types...> const& a, variant<Types...> const& b) {
  if (!bool(b))
    return true;

  if (!bool(a))
    return false;

  return *a >= *b;
}
