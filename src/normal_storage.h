#pragma once

#include "base_storage.h"
#include "visit_table.h"

namespace helper {
/// normal_storage_t
template <typename... Types>
struct normal_storage : base_storage_t<Types...> {
  size_t pos = 0;

  constexpr normal_storage() = default;

  template <size_t Index, typename... Args>
  constexpr explicit normal_storage(in_place_index_t<Index>, Args&&... args)
      : base_storage_t<Types...>(in_place_index<Index>, std::forward<Args>(args)...), pos(Index) {}

  constexpr size_t reset() {
    if (pos != variant_npos)
      return visit_helper::visit_table_indexes([this]<size_t I>(std::integral_constant<size_t, I>){
        size_t prev_index = I;
        this->pos = variant_npos;
        this->template base_reset<I>();

        return prev_index;
      }, *this);

    return pos;
  }

  template <size_t Index, typename... Args>
  constexpr void constructor(Args&&... args) {
    reset();
    this->template base_constructor<Index>(std::forward<Args>(args)...);
    this->pos = Index;
  }

  template <size_t Index, typename T>
  constexpr void set(T&& t) {
    if (reset() == Index)
      this->template base_set<Index>(std::forward<T>(t));
    else
      this->template base_constructor<Index>(std::forward<T>(t));
    this->pos = Index;
  }

  template <typename Other_PS>
  constexpr void first_constructor_from_other(Other_PS&& other) {
    this->pos = variant_npos;
    this->template base_constructor_from_other(other.pos, std::forward<Other_PS>(other));
    this->pos = other.pos;
  }

  template <typename Other_PS>
  constexpr void constructor_from_other(Other_PS&& other) {
    reset();
    this->template base_constructor_from_other(other.pos, std::forward<Other_PS>(other));
    this->pos = other.pos;
  }

  template <typename Other_PS>
  constexpr void set_from_other(Other_PS&& other) {
    if (reset() == other.pos)
      this->template base_set_from_other(other.pos, std::forward<Other_PS>(other));
    else
      this->template base_constructor_from_other(other.pos, std::forward<Other_PS>(other));
    this->pos = other.pos;
  }

  size_t index()
  {
    return pos;
  }

  constexpr static size_t size() noexcept {
    return sizeof...(Types);
  }
};

/// END: normal_storage_t
///==================================================================================================================///
/// storage_t_default_destructible

template <bool is_trivially_destructible, typename... Types>
struct storage_t_default_destructible {};

template <typename... Types>
struct storage_t_default_destructible<true, Types...> : normal_storage<Types...> {
  using normal_storage<Types...>::normal_storage;

  ~storage_t_default_destructible() = default;
};

template <typename... Types>
struct storage_t_default_destructible<false, Types...> : normal_storage<Types...> {
  using normal_storage<Types...>::normal_storage;

  ~storage_t_default_destructible() {
    this->reset();
  };
};

/// END: storage_t_default_destructible
///==================================================================================================================///
} // namespace helper

/// END: helper
///==================================================================================================================///
/// storage_t

template <typename... Types>
using storage_t =
    helper::storage_t_default_destructible<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

/// END: storage_t
///==================================================================================================================///
