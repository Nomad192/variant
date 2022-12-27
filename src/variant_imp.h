#pragma once

#include "multi_union_t.h"

///==================================================================================================================///
/// variant

namespace visit_helper {
template <typename Visitor, typename... Variants>
constexpr auto do_visit(Visitor&& visitor, Variants&&... variants);
}

template <typename... Types>
concept trivially_copy_constructible = std::conjunction_v<std::is_trivially_copy_constructible<Types>...>;

template <typename... Types>
concept trivially_move_constructible = std::conjunction_v<std::is_trivially_move_constructible<Types>...>;

template <typename First, typename... Rest>
struct variant {
private:
  storage_t<First, Rest...> storage;

  using mu_help = multi_union_helper_t<First, Rest...>;

public:
  ///------------------------------------------------------------------------------------///
  /// constructors

  constexpr variant() : variant(in_place_index<0>) {}
  template <size_t N, typename... Args
            //            , typename Valid_Index =
            //                std::enable_if_t<N<sizeof...(Rest) + 1>,
            //                                 typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
            //                                     typename get_type_by_index<N, First, Rest...>::type,
            //                                     Args...>>
            >
  constexpr variant(in_place_index_t<N>, Args&&... args) : storage(in_place_index<N>, std::forward<Args>(args)...) {
    // storage_t<First, Rest...>::template set<N, Args...>(storage, std::forward<Args>(args)...);
  }
  template <typename T, typename... Args
            //      , typename Valid_Type = std::enable_if_t<
            //          get_index_by_type<T, First, Rest...>::index<sizeof...(Rest) + 1>,
            //          typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
            //              T, Args...>>
            >
  constexpr variant(in_place_type_t<T>, Args&&... args)
      : storage(in_place_index<get_index_by_type<T, First, Rest...>::index>, std::forward<Args>(args)...) {
    // storage_t<First, Rest...>::template set<get_index_by_type<T, First, Rest...>::index, Args...>(
    // storage, std::forward<Args>(args)...);
  }

  constexpr variant(const variant& other) requires(trivially_copy_constructible<First, Rest...>) = default;
  //
  //  struct visitor
  //  {
  //    template <typename Arg>
  //    void operator()(Arg&& other_val)
  //    {
  //      multi_union_helper_t<First, Rest...>::template only_set<get_index_by_type<decltype(other_val), First,
  //      Rest...>::index>(storage, other_val);
  //    }
  //  };
  //

  constexpr variant(const variant& other) {
    //    if constexpr (other.index() == 0)
    //    {
    //      new (std::addressof(storage.first)) First(other.storage.first);
    //    }

    storage.index = variant_npos;
    if (other.index() != variant_npos) {
      mu_help::construct_from_other(other.index(), other.storage.value, this->storage.value);
      storage.index = other.index();
    }

    /// wtf aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    //    visit([&](auto other_val) -> void {
    //      multi_union_helper_t<First, Rest...>::template only_set<get_index_by_type<decltype(other_val), First,
    //      Rest...>::index>(storage, other_val); st.index = get_index_by_type<decltype(other_val), First,
    //      Rest...>::index;
    //    }, other);
  }

  constexpr variant(variant&& other) requires(trivially_move_constructible<First, Rest...>) = default;

  constexpr variant(variant&& other) {
    storage.index = variant_npos;
    if (other.index() != variant_npos) {
      mu_help::construct_from_other(other.index(), std::move(other.storage.value), this->storage.value);
      storage.index = other.index();
    }
  }

  template <typename T
            , typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>
            , typename Type = get_type_by_construct_type<T, First, Rest...>
            , size_t Index = get_index_by_type<Type, First, Rest...>::index
            , typename = std::enable_if_t<Index != variant_npos>
            >
  constexpr variant(T&& x) : storage(in_place_index<Index>, std::forward<T>(x))
  {}

  variant& operator=(const variant& other) {
    if (this == &other)
      return *this;

    mu_help::template reset<0>(index(), this->storage.value);
    try {
      if (index() == other.index())
        visit_helper::do_visit(
            [&](auto other_val) -> void {
              multi_union_helper_t<First, Rest...>::template only_operator_set<
                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value,
                                                                                 other_val);
              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
            },
            other);
      else
        visit_helper::do_visit(
            [&](auto other_val) -> void {
              multi_union_helper_t<First, Rest...>::template only_set<
                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value,
                                                                                 other_val);
              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
            },
            other);
    } catch (...) {
      storage.index = variant_npos;
      throw;
    }

    return *this;
  }

  variant& operator=(variant&& other)
  //      noexcept(
  //      std::conjunction_v<std::is_nothrow_move_assignable<Rest>...,
  //      std::is_nothrow_move_assignable<First>>&&
  //          std::conjunction_v<std::is_nothrow_move_constructible<Rest>...,
  //                             std::is_nothrow_move_constructible<First>>)
  {
    if (this == &other)
      return *this;

    mu_help::template reset<0>(index(), this->storage.value);
    try {
      if (index() == other.index())
        visit_helper::do_visit(
            [&](auto other_val) -> void {
              multi_union_helper_t<First, Rest...>::template only_operator_set<
                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value,
                                                                                 std::move(other_val));
              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
            },
            std::move(other));
      else
        visit_helper::do_visit(
            [&](auto other_val) -> void {
              multi_union_helper_t<First, Rest...>::template only_set<
                  get_index_by_type<decltype(other_val), First, Rest...>::index>(storage.value,
                                                                                 std::move(other_val));
              storage.index = get_index_by_type<decltype(other_val), First, Rest...>::index;
            },
            std::move(other));
    } catch (...) {
      storage.index = variant_npos;
      throw;
    }

    return *this;
  }

  template <typename T
            , typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>
            , typename Type = get_type_by_construct_type<T, First, Rest...>
            , size_t Index = get_index_by_type<Type, First, Rest...>::index
            , typename = std::enable_if_t<Index != variant_npos>
                >
  variant& operator=(T&& x) {
    if(index() != Index)
      this->emplace<Index>(Type(std::forward<T>(x)));     /// maybe exception
    else
    {
      try {
        mu_help::template operator_set<Index, T>(index(), storage.value, std::forward<T>(x));
      } catch (...) {
        storage.index = variant_npos;
        throw;
      }
      storage.index = Index;
    }

    return *this;
  }

  /// END: constructors
  ///------------------------------------------------------------------------------------///

  constexpr size_t index() const noexcept {
    return storage.index;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return storage.index == variant_npos;
  }

  template <typename T
            , typename = std::enable_if_t<!std::is_same_v<T, variant<First, Rest...>>>
            , size_t Index = get_index_by_type<T, First, Rest...>::index
            , typename = std::enable_if_t<Index != variant_npos>
            , typename... Args
                >
  void emplace(Args&&... args) {
    try {
      mu_help::template set<Index>(index(), storage.value, std::forward<Args>(args)...);
    } catch (...) {
      storage.index = variant_npos;
      throw;
    }
    storage.index = get_index_by_type<T, First, Rest...>::index;
  }

  template <size_t Index, typename... Args>
  void emplace(Args&&... args) {
    try {
      mu_help::template set<Index>(index(), storage.value, std::forward<Args>(args)...);
    } catch (...) {
      storage.index = variant_npos;
      throw;
    }
    storage.index = Index;
  }

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

  constexpr static size_t size() noexcept {
    return sizeof...(Rest) + 1;
  }

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
      if(this->valueless_by_exception())
      {
        *this = std::move(other);
        other.storage.index = variant_npos;
      }
      else if(other.valueless_by_exception())
      {
        other = std::move(*this);
        storage.index = variant_npos;
      }
      else
      {
        variant<First, Rest...> buffer = std::move(other);
        other = std::move(*this);
        *this = std::move(buffer);
      }
    }
  }
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
