#pragma once

#include <memory>
#include <tuple>

///==================================================================================================================///
/// visit_helper

namespace visit_helper {

///------------------------------------------------------------------------------------///
/// Apply_Indexes
template <typename Visitor, typename... Variants>
struct Apply_Indexes
{
  template <size_t... Indexes>
  constexpr auto operator()(Visitor&& visitor, Variants&&... variants)
  {
    return std::forward<Visitor>(visitor)(std::integral_constant<size_t, Indexes>{}...);
  }
};

/// END: Apply_Indexes
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
using TableReturnType = decltype(std::forward<Visitor>(std::declval<Visitor>())(std::integral_constant<size_t, get_variant_size<Variants>::size>{}...)); /// Visitor Return Type

template <typename ApplyVisitor, typename Visitor, typename... Variants>
using Table_Handler_Function_Pointer = TableReturnType<Visitor, Variants...> (*)(ApplyVisitor&&, Visitor&&,
                                                                      Variants&&...); /// get_func function pointer

///------------------------------------------------------------------------------------///
/// apply_handler

template <size_t... Indexes, typename ApplyVisitor, typename Visitor, typename... Variants>
constexpr auto apply_handler(ApplyVisitor&& apply_visitor, Visitor&& visitor, Variants&&... variants) {

  return std::forward<ApplyVisitor>(apply_visitor)
      .template operator()<Indexes...>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

/// END: apply_handler
///------------------------------------------------------------------------------------///

template <typename FP, typename ApplyVisitor, typename Visitor, typename... Variants>
struct Storage {

  template <typename FirstVariant, typename... RestVariants>
  class Table_Recursive {
    Table_Recursive<RestVariants...> table[get_variant_size<FirstVariant>::size];

    template <size_t... Pre_S, typename Ts, Ts... ints>
    constexpr void make_table(std::integer_sequence<Ts, ints...>) {
      ((table[ints].template make_seq<Pre_S..., ints>()), ...);
    }

  public:
    template <size_t... Pre_S>
    constexpr void make_seq() {
      auto seq = std::make_integer_sequence<size_t, get_variant_size<FirstVariant>::size>{};
      this->template make_table<Pre_S...>(seq);
    }

    template <typename... Rest>
    constexpr FP get_func(size_t first, Rest&&... rest) const {
      return table[first].get_func(rest...);
    }
  };
  template <typename LastVariant>
  class Table_Recursive<LastVariant> {
    FP table[get_variant_size<LastVariant>::size];

    template <size_t... Pre_S, typename Ts, Ts... ints>
    constexpr void make_table(std::integer_sequence<Ts, ints...>) {
      ((table[ints] = &apply_handler<Pre_S..., ints>), ...);
    }

  public:
    template <size_t... Pre_S>
    constexpr void make_seq() {
      auto seq = std::make_integer_sequence<size_t, get_variant_size<LastVariant>::size>{};
      this->template make_table<Pre_S...>(seq);
    }

    constexpr FP get_func(size_t first) const {
      return table[first];
    }
  };

  static constexpr Table_Recursive<Variants...> get_table() {
    Table_Recursive<Variants...> result;
    result.template make_seq<>();
    return result;
  }

  static constexpr Table_Recursive<Variants...> table = get_table();
};

/// END: Storage with Table_Recursive
///------------------------------------------------------------------------------------///
/// visit_table

template <typename Visitor, typename... Variants>
constexpr auto visit_table_indexes(Visitor&& visitor, Variants&&... variants) {
  using A = Apply_Indexes<Visitor, Variants...>;
  using FP = Table_Handler_Function_Pointer<A, Visitor, Variants...>;

  auto handler = Storage<FP, A, Visitor, Variants...>::table.get_func(std::forward<Variants>(variants).index()...);

  A apply;
  return handler(std::forward<A>(apply), std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}


/// END: visit_table
///------------------------------------------------------------------------------------///
} // namespace visit_helper
///==================================================================================================================///
