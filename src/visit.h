#pragma once

#include "get.h"
#include <tuple>

///==================================================================================================================///
/// visit

namespace visit_helper {

///------------------------------------------------------------------------------------///
/// apply_indexes

template <size_t... Indexes, typename Visitor, typename... Variants>
constexpr auto apply_indexes(Visitor&& visitor, Variants&&... variants) {
  return std::forward<Visitor>(visitor)(get<Indexes>(std::forward<Variants>(variants))...);
}

/// END: apply_indexes
///------------------------------------------------------------------------------------///
/// get index from pack

template <size_t I, typename... Pack>
inline constexpr auto& get_val_from_pack(Pack&&... pack) {
  return std::get<sizeof...(Pack) - I - 1>(std::forward_as_tuple(pack...));
}

template <size_t I, typename... VPack>
inline constexpr size_t get_variant_index_from_pack(VPack&&... pack) {
  return get_val_from_pack<I>(std::forward<VPack>(pack)...).index();
}

/// END: get index from pack
///------------------------------------------------------------------------------------///
/// get size from pack

template <size_t I, typename... Pack>
struct get_type_from_pack {
  using type = typename std::tuple_element<I, std::tuple<Pack...>>::type;
};

template <typename Variant>
struct get_variant_size {
  static constexpr size_t size = std::decay_t<Variant>::size();
};

template <size_t I, typename... VPack>
struct get_variant_size_from_pack {
  static constexpr size_t size = get_variant_size<typename get_type_from_pack<I, VPack...>::type>::size;
};

/// END: get size from pack
///------------------------------------------------------------------------------------///
/// Storage with Table_Recursive

template <typename Visitor, typename... Variants>
using ReturnType = decltype(std::forward<Visitor>(std::declval<Visitor>())(
    get<0>(std::forward<Variants>(std::declval<Variants>()))...)); /// Visitor Return Type

template <typename Visitor, typename... Variants>
using Function_Pointer = ReturnType<Visitor, Variants...> (*)(Visitor&&,
                                                              Variants&&...); /// apply_indexes function pointer

template <typename Visitor, typename... Variants>
struct Storage {
  using FP = Function_Pointer<Visitor, Variants...>;

  template <typename FirstVariant, typename... RestVariants>
  struct Table_Recursive {
    Table_Recursive<RestVariants...> table[get_variant_size<FirstVariant>::size];

    template <size_t... Pre_S, typename Ts, Ts... ints>
    constexpr void make_table(std::integer_sequence<Ts, ints...>) {
      ((table[ints].template make_table<Pre_S..., ints>(
           std::make_integer_sequence<size_t, get_variant_size<FirstVariant>::size>{})),
       ...);
    }

    template <typename... Rest>
    constexpr FP get_func(size_t first, Rest&&... rest) {
      return table[first].get_func(rest...);
    }
  };
  template <typename LastVariant>
  struct Table_Recursive<LastVariant> {
    using FP = Function_Pointer<Visitor, Variants...>;

    FP table[get_variant_size<LastVariant>::size];

    template <size_t... Pre_S, typename Ts, Ts... ints>
    constexpr void make_table(std::integer_sequence<Ts, ints...>) {
      ((table[ints] = &apply_indexes<Pre_S..., ints>), ...);
    }

    constexpr FP get_func(size_t first) {
      return table[first];
    }
  };

  Table_Recursive<Variants...> table;
};

/// END: Storage with Table_Recursive
///------------------------------------------------------------------------------------///
/// visit_table

template <typename Visitor, typename... Variants>
constexpr auto visit_table(Visitor&& visitor, Variants&&... variants) {
  using FirstVariant = get_type_from_pack<0, Variants...>::type;
  using FP = Function_Pointer<Visitor, Variants...>;

  Storage<Visitor, Variants...> storage{};
  storage.table.template make_table<>(std::make_integer_sequence<size_t, get_variant_size<FirstVariant>::size>{});

  FP func = storage.table.get_func(std::forward<Variants>(variants).index()...);
  return func(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

/// END: visit_table
///------------------------------------------------------------------------------------///
/// do_visit (some optimizations)

template <typename Visitor, typename... Variants>
constexpr auto do_visit(Visitor&& visitor, Variants&&... variants) { /// Visit optimizations
  if constexpr (sizeof...(Variants) == 0) {                          /// Optimization, the number of variants is zero
    return std::forward<Visitor>(visitor)();
  }
  if constexpr (((get_variant_size<Variants>::size == 1) &&
                 ...)) { /// Optimization, the number of variations in each variant is equal to one
    return apply_indexes<0>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
  }
  /// start at <0, 0> (<index first variant , index current variant>)
  return visit_table<Visitor, Variants...>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

/// END: do_visit (some optimizations)
///------------------------------------------------------------------------------------///
} // namespace visit_helper

template <typename Visitor, typename... Variants>
constexpr auto visit(Visitor&& visitor, Variants&&... variants) {
  return visit_helper::do_visit(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

/// END: visit
///==================================================================================================================///
