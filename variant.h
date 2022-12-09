#pragma once

#include "Multi_Union.h"
#include <functional>
#include <iostream> /// TODO: remove this
#include <tuple>

template <typename First, typename... Rest_Types>
struct variant {

  constexpr variant() : variant(in_place_index<0>) {}
  template <size_t N, typename... Args
            //            , typename Valid_Index =
            //                std::enable_if_t<N<sizeof...(Rest_Types) + 1>,
            //                                 typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
            //                                     typename get_type_by_index<N, First, Rest_Types...>::type,
            //                                     Args...>>
            >
  constexpr variant(in_place_index_t<N>, Args&&... args)
      : current_index(N), storage(in_place_index<N>, std::forward<Args>(args)...) {
    // storage_t<First, Rest_Types...>::template set<N, Args...>(storage, std::forward<Args>(args)...);
  }
  template <typename T, typename... Args
            //      , typename Valid_Type = std::enable_if_t<
            //          get_index_by_type<T, First, Rest_Types...>::index<sizeof...(Rest_Types) + 1>,
            //          typename IS_Constructible = std::enable_if_t<std::is_constructible_v<
            //              T, Args...>>
            >
  constexpr variant(in_place_type_t<T>, Args&&... args)
      : current_index(get_index_by_type<T, First, Rest_Types...>::index),
        storage(in_place_index<get_index_by_type<T, First, Rest_Types...>::index>, std::forward<Args>(args)...) {
    // storage_t<First, Rest_Types...>::template set<get_index_by_type<T, First, Rest_Types...>::index, Args...>(
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
  //      noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Rest_Types>...,
  //                                                           std::is_nothrow_move_constructible<First>>)
  //= delete;
  variant& operator=(const variant&) = delete;
  variant& operator=(variant&&)
      //      noexcept(
      //      std::conjunction_v<std::is_nothrow_move_assignable<Rest_Types>...,
      //      std::is_nothrow_move_assignable<First>>&&
      //          std::conjunction_v<std::is_nothrow_move_constructible<Rest_Types>...,
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
  multi_union_t<First, Rest_Types...> storage;

public:
  template <typename T, typename... Types>
  friend constexpr bool holds_alternative(variant<Types...> const& v);

  constexpr size_t index() const noexcept {
    return current_index;
  }

  template <size_t Index>
  constexpr typename get_type_by_index<Index, First, Rest_Types...>::type& get_from_index() const noexcept {
    return storage.template get<Index>();
  }

  template <typename T>
  constexpr T& get_from_type() const noexcept {
    return storage.template get<get_index_by_type<T, First, Rest_Types...>::index>();
  }

  constexpr static size_t size() noexcept {
    return sizeof...(Rest_Types) + 1;
  }
};

///------------------------------------------------------///

template <typename T, typename... Types>
constexpr bool holds_alternative(variant<Types...> const& v) {
  return get_index_by_type<T, Types...>::index == v.index();
}

template <size_t Index, typename... Types>
constexpr typename get_type_by_index<Index, Types...>::type& get(variant<Types...>& v) {
  return v.template get_from_index<Index>();
}

template <typename T, typename... Types>
constexpr T& get(const variant<Types...>& v) {
  return v.template get_from_type<T>();
}
//
// template <typename First, typename... Rest>
// struct var
//{
//  template <size_t Index, typename Visitor, typename Tuple, typename... Variants>
//  constexpr static auto visit_(Visitor&& visitor, Tuple&& tuple, variant<First, Rest...>&& first_variant,
//  Variants&&... rest_variants) noexcept
//  {
//    if(Index == first_variant.index())
//    {
//
//      std::tuple new_arg(first_variant.template get_from_index<Index>());
//      auto new_tuple = std::tuple_cat(tuple, new_arg);
//
//      if constexpr (sizeof...(Variants) > 0)
//        return var<variant<First, Rest...>>::template visit_<0>(visitor, std::move(new_tuple),
//        std::forward<Variants>(rest_variants)...);
//      else
//        return std::apply(std::forward<Visitor>(visitor), std::move(new_tuple));
//    }
//    else
//    {
//      return var<variant<First, Rest...>>::template visit_<Index + 1>(visitor, std::move(tuple), first_variant,
//      std::forward<Variants>(rest_variants)...);
//    }
//  }
//};
//
//template <class T>
//constexpr T&& as_move_ref(T& v) {
//  return std::move(v);
//}

template <typename TupleT, typename Fn>
auto for_each_tuple2(TupleT&& tp, Fn&& fn) {
  return std::apply([&fn](auto&&... args) { return (fn(std::forward<std::decay_t<decltype(args)>>(args)), ...); },
                    std::forward<TupleT>(tp));
}

template <size_t Index, typename Visitor, typename Tuple, typename FV, typename... Variants>
constexpr static decltype(auto) visit_(Visitor&& visitor, Tuple&& tuple, FV&& first_variant,
                                       Variants&&... rest_variants) noexcept {
  if (Index == first_variant.index()) {
    auto new_argv = std::forward_as_tuple(std::forward<FV>(first_variant).template get_from_index<Index>());

    if constexpr (sizeof...(Variants) > 0) {
      return visit_<0>(visitor, std::tuple_cat(tuple, new_argv), std::forward<Variants>(rest_variants)...);
    } else {
      return for_each_tuple2(std::tuple_cat(tuple, new_argv), std::forward<Visitor>(visitor));
    }
  } else if constexpr (Index + 1 < std::decay_t<decltype(first_variant)>::size()) {
    return visit_<Index + 1>(std::forward<Visitor>(visitor), tuple, std::forward<FV>(first_variant),
                             std::forward<Variants>(rest_variants)...);
  }
}

template <typename Visitor, typename... Variants>
constexpr static auto visit(Visitor&& visitor, Variants&&... variants) {
  if constexpr (sizeof...(Variants) == 0) {
    return std::apply(std::forward<Visitor>(visitor));
  }

  return visit_<0>(std::forward<Visitor>(visitor), std::tuple<>(), std::forward<Variants>(variants)...);
}
