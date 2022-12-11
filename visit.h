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
/// visit_recursive

/// start at <0, 0> (<index first variant , index current variant>)
template <size_t N, size_t Current_Variant, size_t... Indexes, typename Visitor, typename... Variants>
constexpr auto visit_recursive(Visitor&& visitor, Variants&&... variants) {
  if (N == get_variant_index_from_pack<Current_Variant, Variants...>(std::forward<Variants>(variants)...)) {
    if constexpr (Current_Variant + 1 < sizeof...(Variants)) {
      return visit_recursive<0, Current_Variant + 1, N, Indexes...>(std::forward<Visitor>(visitor),
                                                                    std::forward<Variants>(variants)...);
    } else {
      return apply_indexes<N, Indexes...>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
    }
  }

  if constexpr (N + 1 < get_variant_size_from_pack<Current_Variant, Variants...>::size) {
    return visit_recursive<N + 1, Current_Variant, Indexes...>(std::forward<Visitor>(visitor),
                                                               std::forward<Variants>(variants)...);
  }
  assert(false); /// so that the compiler doesn't complain about the non-void function TODO: remove?
}

/// END: visit_recursive
///------------------------------------------------------------------------------------///
/// do_visit (some optimizations)

template <typename Visitor, typename... Variants>
constexpr auto do_visit(Visitor&& visitor, Variants&&... variants) {
  if constexpr (sizeof...(Variants) == 0) {
    return std::forward<Visitor>(visitor)();
  }
  if constexpr (((get_variant_size<Variants>::size == 1) && ...)) {
    return apply_indexes<0>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
  }
  /// start at <0, 0> (<index first variant , index current variant>)
  return visit_recursive<0, 0>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
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
