#pragma once

#include "multi_union_t.h"

///==================================================================================================================///
/// variant

template <typename... Types>
concept trivially_copy_constructible = std::conjunction_v<std::is_trivially_copy_constructible<Types>...>;

template <typename... Types>
concept trivially_move_constructible = std::conjunction_v<std::is_trivially_move_constructible<Types>...>;


template <typename First, typename... Rest>
struct variant {
private:
  size_t current_index = 0;
  multi_union_t<First, Rest...> storage;

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
  constexpr variant(in_place_index_t<N>, Args&&... args)
      : current_index(N), storage(in_place_index<N>, std::forward<Args>(args)...) {
    // storage_t<First, Rest...>::template set<N, Args...>(storage, std::forward<Args>(args)...);
  }
  template <typename T, typename... Args
            //      , typename Valid_Type = std::enable_if_t<
            //          get_index_by_type<T, First, Rest...>::index<sizeof...(Rest) + 1>,
            //          typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
            //              T, Args...>>
            >
  constexpr variant(in_place_type_t<T>, Args&&... args)
      : current_index(get_index_by_type<T, First, Rest...>::index),
        storage(in_place_index<get_index_by_type<T, First, Rest...>::index>, std::forward<Args>(args)...) {
    // storage_t<First, Rest...>::template set<get_index_by_type<T, First, Rest...>::index, Args...>(
    // storage, std::forward<Args>(args)...);
  }
  template <typename T>
  constexpr variant(T x) : variant(in_place_type<T>, std::forward<T>(x)) {}


  constexpr variant(const variant& other) requires(trivially_copy_constructible<First, Rest...>) = default;
//
//  struct visiter
//  {
//    template <typename Arg>
//    void operator()(Arg&& other_val)
//    {
//      multi_union_helper_t<First, Rest...>::template only_set<get_index_by_type<decltype(other_val), First, Rest...>::index>(storage, other_val);
//    }
//  };
//

  constexpr variant(const variant& other)
  {
//    if constexpr (other.index() == 0)
//    {
//      new (std::addressof(storage.first)) First(other.storage.first);
//    }

    current_index = -1;
    if(other.index() != -1) {
      mu_help::template construct_from_other(other.index(), other.storage, this->storage);
      current_index = other.index();
    }

      ///wtf aaaaaaaaaaaaaaaaaaaaaa
//    visit([&](auto other_val) -> void {
//      multi_union_helper_t<First, Rest...>::template only_set<get_index_by_type<decltype(other_val), First, Rest...>::index>(storage, other_val);
//      current_index = get_index_by_type<decltype(other_val), First, Rest...>::index;
//    }, other);
  }

  constexpr variant(variant&& other) requires(trivially_move_constructible<First, Rest...>) = default;

  constexpr variant(variant&& other) {
    current_index = -1;
    if(other.index() != -1) {
      mu_help::template construct_from_other(other.index(), std::move(other.storage), this->storage);
      current_index = other.index();
    }
  }
    //    auto stored = visit([](auto& stored) {
    //      return stored;
    //    }, other);
    //
    //    visit(
    //        [](auto&& arg) {
    //          using T = std::decay_t<decltype(arg)>;
    //          if constexpr (std::is_same_v<T, int>)
    //            std::cout << "int with value " << arg << '\n';
    //          else if constexpr (std::is_same_v<T, float>)
    //            std::cout << "long with value " << arg << '\n';
    //          else if constexpr (std::is_same_v<T, double>)
    //            std::cout << "double with value " << arg << '\n';
    //          ;
    //        },
    //        other);

    //    current_index = get_index_by_type<decltype(stored)>::index;
    //    std::cout << "ok " << stored << " " << current_index << std::endl;
  //}
  //      noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Rest>...,
  //                                                           std::is_nothrow_move_constructible<First>>)
  //= delete;
  variant& operator=(const variant&)= delete;
  variant& operator=(variant&&)
      //      noexcept(
      //      std::conjunction_v<std::is_nothrow_move_assignable<Rest>...,
      //      std::is_nothrow_move_assignable<First>>&&
      //          std::conjunction_v<std::is_nothrow_move_constructible<Rest>...,
      //                             std::is_nothrow_move_constructible<First>>)
      = delete;

  /// END: constructors
  ///------------------------------------------------------------------------------------///

  constexpr size_t index() const noexcept {
    return current_index;
  }

  template <typename T>
  void emplace(T x)
  {
    current_index = -1;
    mu_help::template set<get_index_by_type<T, First, Rest...>::index, T>(index(), storage, std::forward<T>(x));
    current_index = get_index_by_type<T, First, Rest...>::index;
  }

  template <size_t Index, typename T>
  void emplace(T x)
  {
    current_index = -1;
    mu_help::template set<Index, T>(index(), storage, std::forward<T>(x));
    current_index = Index;
  }

  ///------------------------------------------------------------------------------------///
  /// get_from_index

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>& get_from_index() & {
    if (Index != index())
      throw bad_variant_access();
    return multi_union_helper_t<First, Rest...>::template get<Index>(this->storage);
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>&& get_from_index() && {
    if (Index != index())
      throw bad_variant_access();
    return std::move(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage));
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const& get_from_index() const& {
    if (Index != index())
      throw bad_variant_access();
    return multi_union_helper_t<First, Rest...>::template get<Index>(this->storage);
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const&& get_from_index() const&& {
    if (Index != index())
      throw bad_variant_access();
    return std::move(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage));
  }

  /// END: get_from_index
  ///------------------------------------------------------------------------------------///
  /// get_if_from_index

  template <std::size_t Index>
  constexpr std::add_pointer_t<std::variant_alternative_t<Index, std::variant<First, Rest...>>>
  get_if_from_index() noexcept {
    if (Index != index())
      return nullptr;
    return std::addressof(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage));
  }

  template <std::size_t Index>
  constexpr std::add_pointer_t<const std::variant_alternative_t<Index, std::variant<First, Rest...>>>
  get_if_from_index() const noexcept {
    if (Index != index())
      return nullptr;
    return std::addressof(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage));
  }

  /// END: get_if_from_index
  ///------------------------------------------------------------------------------------///

  constexpr static size_t size() noexcept {
    return sizeof...(Rest) + 1;
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
