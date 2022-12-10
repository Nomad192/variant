#pragma once

#include "multi_union_t.h"
#include <functional> /// TODO: remove this?
#include <iostream>   /// TODO: remove this!
#include <tuple>      /// TODO: remove this??

///==================================================================================================================///
/// variant

template <typename First, typename... Rest>
struct variant {

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
  constexpr variant(T x) : variant(in_place_type<T>, x) {}

  constexpr variant(const variant& other) = default;
  constexpr variant(variant&& other) {
    //    auto stored = visit([](auto& stored) {
    //      return stored;
    //    }, other);
    //
    std::cout << "aaaa" << std::endl;
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
  }
  //      noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Rest>...,
  //                                                           std::is_nothrow_move_constructible<First>>)
  //= delete;
  variant& operator=(const variant&) = delete;
  variant& operator=(variant&&)
      //      noexcept(
      //      std::conjunction_v<std::is_nothrow_move_assignable<Rest>...,
      //      std::is_nothrow_move_assignable<First>>&&
      //          std::conjunction_v<std::is_nothrow_move_constructible<Rest>...,
      //                             std::is_nothrow_move_constructible<First>>)
      = delete;

private:
  //  template <size_t N = 0>
  //  struct current_index_t {
  //    template <size_t I>
  //    explicit constexpr current_index_t(in_place_index_t<I>) : index(I) {}
  //    explicit constexpr current_index_t(size_t i) : index(i) {}
  //
  //    size_t index = N;
  //  };

  // current_index_t<0> current_index;

  size_t current_index = 0;
  multi_union_t<First, Rest...> storage;

public:
  //  template <typename T, typename... Types>
  //  friend constexpr bool holds_alternative(variant<Types...> const& v);

  constexpr size_t index() const noexcept {
    return current_index;
  }

  ///------------------------------------------------------------------------------------///

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>& get_from_index() & {
    //    if (Index != index())
    //      throw bad_variant_access();
    return multi_union_helper_t<First, Rest...>::template get<Index>(this->storage);
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>>&& get_from_index() && {
    //    if (Index != index())
    //      throw bad_variant_access();
    return std::move(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage));
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const& get_from_index() const& {
    //    if (Index != index())
    //      throw bad_variant_access();
    return multi_union_helper_t<First, Rest...>::template get<Index>(this->storage);
  }

  template <size_t Index>
  constexpr variant_alternative_t<Index, variant<First, Rest...>> const&& get_from_index() const&& {
    //    if (Index != index())
    //      throw bad_variant_access();
    return std::move(multi_union_helper_t<First, Rest...>::template get<Index>(this->storage));
  }

  ///------------------------------------------------------------------------------------///

  template <typename T>
  constexpr decltype(auto) get_from_type() const noexcept {
    return get_from_index<get_index_by_type<T, First, Rest...>::index>();
  }

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

///------------------------------------------------------------------------------------///
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
/// visit

// template <typename TupleT, typename Fn>
// auto for_each_tuple2(TupleT&& tp, Fn&& fn) {
//   return std::apply([&fn](auto&&... args) { return (fn(std::forward<std::decay_t<decltype(args)>>(args)), ...); },
//                     std::forward<TupleT>(tp));
// }

// template <size_t N = 0, typename Visitor, typename Tuple, typename... Args>
// auto tuple_apply(Visitor&& visitor, Tuple&& tuple, Args&&... args)
//{
//   if constexpr (N == std::tuple_size<Tuple>::value)
//   {
//     return std::forward<Visitor>(visitor)(std::forward<Args>(args)...);
//   }
//   else
//   {
//     return tuple_apply<N+1, Visitor, Tuple, Args...,
//     std::decay_t<decltype(std::get<N>(tuple))>>(std::forward<Visitor>(visitor), std::forward<Tuple>(tuple),
//     std::forward<Args>(args)..., std::get<N>(tuple));
//   }
// }

///------------------------------------------------------------------------------------///
//
// template <size_t Index_in_cur_variant, size_t Cur_variant, typename Visitor, size_t... Indexes, typename... Variants>
// constexpr static decltype(auto) visit_(Visitor&& visitor, std::index_sequence<Indexes...>, Variants&&... variants)
// noexcept {
//
//  if (Index_in_cur_variant == first_variant.index()) {
//    //auto new_argv = std::forward_as_tuple(get<Index>(std::forward<FV>(first_variant)));
//
//    if constexpr (Cur_variant + 1 < sizeof...(Variants)) {
//      return visit_<0, Cur_variant + 1>(visitor, std::index_sequence<Index_in_cur_variant, Indexes...>(),
//      std::forward<Variants>(variants)...);
//    } else {
//      std::forward<Visitor>(visitor)(get<Index_in_cur_variant, Indexes...>(std::forward<Variants>(variants)...));
//    }
//  }
//
//  if constexpr (Index_in_cur_variant + 1 < std::decay_t<FV>::size()) {
//    return visit_<Index_in_cur_variant + 1>(std::forward<Visitor>(visitor),  std::index_sequence<Indexes...>(),
//    std::forward<Variants>(variants)...);
//  }
//  assert(false); /// TODO: remove?
//}

///------------------------------------------------------------------------------------///

template <size_t... Indexes, typename Visitor, typename... Variants>
constexpr auto visit__(Visitor&& visitor, Variants&&... variants) {
  return std::forward<Visitor>(visitor)(get<Indexes>(std::forward<Variants>(variants))...);
}

template <size_t N, size_t Var, size_t... Indexes, typename Visitor, typename... Variants>
constexpr auto visit_(Visitor&& visitor, Variants&&... variants) {
  if (N == std::get<sizeof...(Variants) - Var - 1>(std::forward_as_tuple(variants...)).index()) {
    size_t aaa = N;

    /// std::cout << aaa << std::endl; TODO: remove this
    if constexpr (Var + 1 < sizeof...(Variants)) {
      return visit_<0, Var + 1, N, Indexes...>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
    } else {
      return visit__<N, Indexes...>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
    }
  }

  if constexpr (N + 1 < std::decay_t<typename std::tuple_element<Var, std::tuple<Variants...>>::type>::size()) {
    return visit_<N + 1, Var, Indexes...>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
  }
  assert(false); /// TODO: remove?
}

template <typename Visitor, typename... Variants>
constexpr auto visit(Visitor&& visitor, Variants&&... variants) {
  if constexpr (sizeof...(Variants) == 0) {
    return std::forward<Visitor>(visitor)();
  }

  return visit_<0, 0>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
  // return visit_(std::forward<Visitor>(visitor),
  // std::index_sequence<variant_index_f<Variants>(std::forward<Variants>(variants))...>{},
  // std::forward<Variants>(variants)...);
}

/// END: visit
///==================================================================================================================///
/// END: Other Functions
///==================================================================================================================///
