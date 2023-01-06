#pragma once

#include "concepts.h"
#include "multi_union_t.h"

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

  constexpr variant(const variant& other) requires(trivially_copy_constructible<First, Rest...>) = default;

  constexpr variant(const variant& other) noexcept(nothrow_copy_constructible<First, Rest...>)
      requires(copy_constructible<First, Rest...> && !trivially_copy_constructible<First, Rest...>) {
    storage.constructor_from_other(other.storage);

    //    set_index(variant_npos);
    //    if (other.index() != variant_npos) {
    //      mu_help::construct_from_other(other.index(), other.storage.value, this->storage.value);
    //      set_index(other.index());
    //    }
  }

  constexpr variant(variant&& other) requires(trivially_move_constructible<First, Rest...>) = default;

  constexpr variant(variant&& other) noexcept(nothrow_move_constructible<First, Rest...>)
      requires(move_constructible<First, Rest...> && !trivially_move_constructible<First, Rest...>) {
    storage.constructor_from_other(std::move(other).storage);

    //    set_index(variant_npos);
    //    if (other.index() != variant_npos) {
    //      mu_help::construct_from_other(other.index(), std::move(other.storage.value), this->storage.value);
    //      set_index(other.index());
    //    }
  }

  template <typename T, typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>,
            typename Type = get_type_by_construct_type<T, First, Rest...>,
            size_t Index = get_index_by_type<Type, First, Rest...>::index,
            typename = std::enable_if_t<Index != variant_npos>>
  constexpr variant(T&& x) noexcept(std::is_nothrow_constructible_v<Type, T>) requires(std::is_constructible_v<Type, T>)
      : storage(in_place_index<Index>, std::forward<T>(x)) {}

  /// END: constructors
  ///------------------------------------------------------------------------------------///
  /// operator=

  variant& operator=(const variant& other) noexcept
      requires(trivially_copy_assignable<First, Rest...>&& trivially_copy_constructible<First, Rest...>) = default;

  variant& operator=(const variant& other) noexcept(
      nothrow_copy_assignable<First, Rest...>&& nothrow_copy_constructible<First, Rest...>)
      requires((copy_assignable<First, Rest...> &&
                copy_constructible<First, Rest...>)&&!(trivially_copy_assignable<First, Rest...> &&
                                                       trivially_copy_constructible<First, Rest...>)) {
    if (this == &other)
      return *this;

    storage.set_from_other(other.storage);

    //    mu_help::template reset<0>(index(), this->storage.value);
    ////    try {
    //      if (index() == other.index())
    //      {
    //        set_index(variant_npos);
    //        visit_helper::do_visit(
    //            [&](auto other_val) -> void {
    //              multi_union_helper_t<First, Rest...>::template only_operator_set<
    //                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value, other_val);
    //              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
    //            },
    //            other);
    //      }
    //      else
    //      {
    //        set_index(variant_npos);
    //        visit_helper::do_visit(
    //            [&](auto other_val) -> void {
    //              multi_union_helper_t<First, Rest...>::template only_set<
    //                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value, other_val);
    //              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
    //            },
    //            other);
    //      }
    //    } catch (...) {
    //      storage.index = variant_npos;
    //    }

    return *this;
  }

  variant& operator=(variant&& other) noexcept
      requires(trivially_move_assignable<First, Rest...>&& trivially_move_constructible<First, Rest...>) = default;

  variant& operator=(variant&& other) noexcept(
      nothrow_move_assignable<First, Rest...>&& nothrow_move_constructible<First, Rest...>)
      requires((move_assignable<First, Rest...> &&
                move_constructible<First, Rest...>)&&!(trivially_move_assignable<First, Rest...> &&
                                                       trivially_move_constructible<First, Rest...>)) {
    if (this == &other)
      return *this;

    storage.set_from_other(std::move(other).storage);

    // mu_help::template reset<0>(index(), this->storage.value);
    //  storage.index = variant_npos;
    //    try {
    //      if (index() == other.index())
    //      {
    //        storage.index = variant_npos;
    //        visit_helper::do_visit(
    //            [&](auto other_val) -> void {
    //              multi_union_helper_t<First, Rest...>::template only_operator_set<
    //                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value,
    //                  std::move(other_val));
    //              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
    //            },
    //            std::move(other));
    //      }
    //      else
    //      {
    //        storage.index = variant_npos;
    //        visit_helper::do_visit(
    //            [&](auto other_val) -> void {
    //              multi_union_helper_t<First, Rest...>::template only_set<
    //                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value,
    //                  std::move(other_val));
    //              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
    //            },
    //            std::move(other));
    //      }

    //    } catch (...) {
    //      storage.index = variant_npos;
    //      throw;
    //    }

    return *this;
  }

  template <typename T, typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>,
            typename Type = get_type_by_construct_type<T, First, Rest...>,
            size_t Index = get_index_by_type<Type, First, Rest...>::index,
            typename = std::enable_if_t<Index != variant_npos>>
  variant& operator=(T&& t) noexcept(std::is_nothrow_assignable_v<Type, T>) requires(std::is_assignable_v<Type, T>) {

    if (index() != Index)
      storage.template constructor<Index>(Type(std::forward<T>(t)));
    else
      storage.template set<Index>(std::forward<T>(t));

    //    if (index() != Index)
    //      this->emplace<Index>(Type(std::forward<T>(x))); /// maybe exception
    //    else {
    //      try {
    //        mu_help::template operator_set<Index, T>(index(), storage.value, std::forward<T>(x));
    //      } catch (...) {
    //        storage.index = variant_npos;
    //        throw;
    //      }
    //      storage.index = Index;
    //    }

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
    //    try {
    //      mu_help::template set<Index>(index(), storage.value, std::forward<Args>(args)...);
    //    } catch (...) {
    //      storage.index = variant_npos;
    //      throw;
    //    }
    //    storage.index = get_index_by_type<T, First, Rest...>::index;
  }

  template <size_t Index, typename... Args>
  void emplace(Args&&... args) {

    storage.template constructor<Index>(std::forward<Args>(args)...);
    //    try {
    //      mu_help::template set<Index>(index(), storage.value, std::forward<Args>(args)...);
    //    } catch (...) {
    //      storage.index = variant_npos;
    //      throw;
    //    }
    //    storage.index = Index;
  }

  /// END: emplace
  ///------------------------------------------------------------------------------------///
  /// get_from_index

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>& get_from_index() & {
    if (Index != index())
      throw bad_variant_access();
    return multi_union_helper_t<First, Rest...>::template get<Index>(this->storage.value);
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>&& get_from_index() && {
    if (Index != index())
      throw bad_variant_access();
    return std::move(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage.value));
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const& get_from_index() const& {
    if (Index != index())
      throw bad_variant_access();
    return multi_union_helper_t<First, Rest...>::template get<Index>(this->storage.value);
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const&& get_from_index() const&& {
    if (Index != index())
      throw bad_variant_access();
    return std::move(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage.value));
  }

  /// END: get_from_index
  ///------------------------------------------------------------------------------------///
  /// get_if_from_index

  template <std::size_t Index>
  constexpr std::add_pointer_t<variant_alternative_t<Index, variant<First, Rest...>>> get_if_from_index() noexcept {
    if (Index != index())
      return nullptr;
    return std::addressof(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage.value));
  }

  template <std::size_t Index>
  constexpr std::add_pointer_t<const variant_alternative_t<Index, variant<First, Rest...>>>
  get_if_from_index() const noexcept {
    if (Index != index())
      return nullptr;
    return std::addressof(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage.value));
  }

  /// END: get_if_from_index
  ///------------------------------------------------------------------------------------///
  /// swap

  constexpr void swap(variant& other) {
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
        std::swap(storage.index, other.storage.index);
      }
      return;
    } else {
      if (this->valueless_by_exception()) {
        *this = std::move(other);
        other.storage.index = variant_npos;
      } else if (other.valueless_by_exception()) {
        other = std::move(*this);
        storage.index = variant_npos;
      } else {
        variant<First, Rest...> buffer = std::move(other);
        other = std::move(*this);
        *this = std::move(buffer);
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
    return storage.index == variant_npos;
  }

  constexpr size_t index() const noexcept {
    return storage.index;
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
