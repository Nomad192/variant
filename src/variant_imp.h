#pragma once

#include "concepts.h"
#include "normal_storage.h"

///==================================================================================================================///
/// do_visit declaration

/**
 * I can't use my visit here because compiler can't choose from my and the standard visit
 **/

namespace visit_helper {
template <typename Visitor, typename... Variants>
constexpr auto do_visit(Visitor&& visitor, Variants&&... variants);
}

/// END: do_visit declaration
///==================================================================================================================///
/// variant

template <typename First, typename... Rest>
struct variant {
private:
  storage_t<First, Rest...> storage;

public:
  ///------------------------------------------------------------------------------------///
  /// constructors

  constexpr variant() noexcept(std::conjunction_v<std::is_nothrow_default_constructible<First>>)
      requires(std::is_default_constructible_v<First>)
      : variant(in_place_index<0>) {}
  template <
      size_t N, typename... Args, typename = std::enable_if_t<N <= sizeof...(Rest)>,
      typename = std::enable_if_t<std::is_constructible_v<variant_alternative_t<N, variant<First, Rest...>>, Args...>>>
  constexpr variant(in_place_index_t<N>, Args&&... args) : storage(in_place_index<N>, std::forward<Args>(args)...) {}

  template <typename T, typename... Args, size_t N = get_index_by_type<T, First, Rest...>::index,
            typename = std::enable_if_t<N <= sizeof...(Rest)>,
            typename = std::enable_if_t<std::is_constructible_v<T, Args...>>>
  constexpr variant(in_place_type_t<T>, Args&&... args) : storage(in_place_index<N>, std::forward<Args>(args)...) {}

  constexpr variant(const variant& other) requires(helper::trivially_copy_constructible<First, Rest...>) = default;

  constexpr variant(const variant& other) noexcept(helper::nothrow_copy_constructible<First, Rest...>)
      requires(helper::copy_constructible<First, Rest...> && !helper::trivially_copy_constructible<First, Rest...>) {
    storage.first_constructor_from_other(other.storage);
  }

  constexpr variant(variant&& other) requires(helper::trivially_move_constructible<First, Rest...>) = default;

  constexpr variant(variant&& other) noexcept(helper::nothrow_move_constructible<First, Rest...>)
      requires(helper::move_constructible<First, Rest...> && !helper::trivially_move_constructible<First, Rest...>) {
    storage.first_constructor_from_other(std::move(other).storage);
  }

  template <typename T, typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>,
            typename T_j = get_type_by_construct_type<T, First, Rest...>,
            size_t Index = get_index_by_type<T_j, First, Rest...>::index,
            typename = std::enable_if_t<Index != variant_npos>>
  constexpr variant(T&& x) noexcept(std::is_nothrow_constructible_v<T_j, T>) requires(std::is_constructible_v<T_j, T>)
      : storage(in_place_index<Index>, std::forward<T>(x)) {}

  /// END: constructors
  ///------------------------------------------------------------------------------------///
  /// operator=

  variant& operator=(const variant& other) noexcept
      requires(helper::trivially_copy_assignable<First, Rest...>&&
                   helper::trivially_copy_constructible<First, Rest...>) = default;

  variant& operator=(const variant& other) noexcept(
      helper::nothrow_copy_assignable<First, Rest...>&& helper::nothrow_copy_constructible<First, Rest...>)
      requires((helper::copy_assignable<First, Rest...> &&
                helper::copy_constructible<First, Rest...>)&&!(helper::trivially_copy_assignable<First, Rest...> &&
                                                               helper::trivially_copy_constructible<First, Rest...>)) {
    if (this == &other)
      return *this;

    if (other.valueless_by_exception()) {
      storage.reset();
      return *this;
    }

    visit_helper::visit_table_indexes(
        [this, &other](auto index1, auto index2) {
          if constexpr (index1() == index2()) {
            storage.template base_set_from_other<index2()>(other.storage);
          } else {
            visit_helper::do_visit(
                [&, this](auto&& rhs) {
                  using rhs_t = std::decay_t<decltype(rhs)>;
                  if constexpr (std::is_nothrow_copy_constructible_v<rhs_t> ||
                                !std::is_nothrow_move_constructible_v<rhs_t>)
                    this->emplace<rhs_t>(rhs);
                  else
                    *this = variant(other);
                },
                other);
          }
        },
        this->storage, other.storage);

    return *this;
  }

  variant& operator=(variant&& other) noexcept
      requires(helper::trivially_move_assignable<First, Rest...>&&
                   helper::trivially_move_constructible<First, Rest...>) = default;

  variant& operator=(variant&& other) noexcept(
      helper::nothrow_move_assignable<First, Rest...>&& helper::nothrow_move_constructible<First, Rest...>)
      requires((helper::move_assignable<First, Rest...> &&
                helper::move_constructible<First, Rest...>)&&!(helper::trivially_move_assignable<First, Rest...> &&
                                                               helper::trivially_move_constructible<First, Rest...>)) {
    if (this == &other)
      return *this;

    if (other.valueless_by_exception()) {
      storage.reset();
      return *this;
    }

    storage.set_from_other(std::move(other).storage);

    return *this;
  }

  template <typename T, typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>,
            typename T_j = get_type_by_construct_type<T, First, Rest...>,
            size_t Index = get_index_by_type<T_j, First, Rest...>::index,
            typename = std::enable_if_t<Index != variant_npos>>
  variant& operator=(T&& t) noexcept(std::is_nothrow_assignable_v<T_j, T>) requires(std::is_assignable_v<T_j, T>) {
    if (index() != Index) {
      if constexpr (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>) {
        storage.template constructor<Index>(std::forward<T>(t));
      } else {
        storage.template constructor<Index>(T_j(std::forward<T>(t)));
      }
    } else {
      storage.template set<Index>(std::forward<T>(t));
    }
    return *this;
  }

  /// END: operator=
  ///------------------------------------------------------------------------------------///
  /// emplace

  template <typename T, typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>,
            size_t Index = get_index_by_type<T, First, Rest...>::index,
            typename = std::enable_if_t<Index != variant_npos>, typename... Args>
  void emplace(Args&&... args) {
    storage.template constructor<Index>(std::forward<Args>(args)...);
  }

  template <size_t Index, typename... Args>
  void emplace(Args&&... args) {
    storage.template constructor<Index>(std::forward<Args>(args)...);
  }

  /// END: emplace
  ///------------------------------------------------------------------------------------///
  /// get_from_index

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>& get_from_index() & {
    if (Index != index())
      throw bad_variant_access();
    return storage.template get<Index>();
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>&& get_from_index() && {
    if (Index != index())
      throw bad_variant_access();
    return std::move(storage.template get<Index>());
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const& get_from_index() const& {
    if (Index != index())
      throw bad_variant_access();
    return storage.template get<Index>();
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const&& get_from_index() const&& {
    if (Index != index())
      throw bad_variant_access();
    return std::move(storage.template get<Index>());
  }

  /// END: get_from_index
  ///------------------------------------------------------------------------------------///
  /// get_if_from_index

  template <std::size_t Index>
  constexpr std::add_pointer_t<variant_alternative_t<Index, variant<First, Rest...>>> get_if_from_index() noexcept {
    if (Index != index())
      return nullptr;
    return std::addressof(storage.template get<Index>());
  }

  template <std::size_t Index>
  constexpr std::add_pointer_t<const variant_alternative_t<Index, variant<First, Rest...>>>
  get_if_from_index() const noexcept {
    if (Index != index())
      return nullptr;
    return std::addressof(storage.template get<Index>());
  }

  /// END: get_if_from_index
  ///------------------------------------------------------------------------------------///
  /// swap

  constexpr void swap(variant& other) noexcept(
      std::is_nothrow_move_constructible_v<First>&& std::is_nothrow_swappable_v<First> &&
      ((std::is_nothrow_move_constructible_v<Rest> && std::is_nothrow_swappable_v<Rest>)&&...)) {
    if (this == &other)
      return;

    if (index() == other.index()) {
      if (!this->valueless_by_exception()) {
        visit_helper::do_visit(
            [&](auto&& a, auto&& b) {
              using std::swap;
              if constexpr (std::is_same_v<decltype(a), decltype(b)>)
                swap(a, b);
            },
            *this, other);
        std::swap(storage.pos, other.storage.pos);
      }
      return;
    } else {
      if (this->valueless_by_exception()) {
        *this = std::move(other);
        other.storage.pos = variant_npos;
      } else if (other.valueless_by_exception()) {
        other = std::move(*this);
        storage.pos = variant_npos;
      } else {
        std::swap(*this, other);
      }
    }
  }

  /// END: swap
  ///------------------------------------------------------------------------------------///
  /// Other variant Functions

  constexpr static size_t size() noexcept {
    return sizeof...(Rest) + 1;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

  constexpr size_t index() const noexcept {
    return storage.pos;
  }

  /// END: Other variant Functions
  ///------------------------------------------------------------------------------------///
};

/// END: variant
///==================================================================================================================///
/// Other Functions

template <typename T, typename... Types>
constexpr bool holds_alternative(variant<Types...> const& v) {
  return get_index_by_type<T, Types...>::index == v.index();
}

/// END: Other Functions
///==================================================================================================================///
