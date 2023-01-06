#pragma once
#include <utility>

namespace helper {
///==================================================================================================================///
/// concepts

template <typename... Types>
concept trivially_copy_constructible = std::conjunction_v<std::is_trivially_copy_constructible<Types>...>;

template <typename... Types>
concept trivially_move_constructible = std::conjunction_v<std::is_trivially_move_constructible<Types>...>;

///------------------------------------------------------------------------------------------------------------------///

template <typename... Types>
concept copy_constructible = std::conjunction_v<std::is_copy_constructible<Types>...>;

template <typename... Types>
concept nothrow_copy_constructible = std::conjunction_v<std::is_nothrow_copy_constructible<Types>...>;

///------------------------------------------------------------------------------------------------------------------///

template <typename... Types>
concept move_constructible = std::conjunction_v<std::is_move_constructible<Types>...>;

template <typename... Types>
concept nothrow_move_constructible = std::conjunction_v<std::is_nothrow_move_constructible<Types>...>;

///==================================================================================================================///

template <typename... Types>
concept trivially_copy_assignable = std::conjunction_v<std::is_trivially_copy_assignable<Types>...>;

template <typename... Types>
concept trivially_move_assignable = std::conjunction_v<std::is_trivially_move_assignable<Types>...>;

///------------------------------------------------------------------------------------------------------------------///

template <typename... Types>
concept copy_assignable = std::conjunction_v<std::is_copy_assignable<Types>...>;

template <typename... Types>
concept nothrow_copy_assignable = std::conjunction_v<std::is_nothrow_copy_assignable<Types>...>;

///------------------------------------------------------------------------------------------------------------------///

template <typename... Types>
concept move_assignable = std::conjunction_v<std::is_move_assignable<Types>...>;

template <typename... Types>
concept nothrow_move_assignable = std::conjunction_v<std::is_nothrow_move_assignable<Types>...>;

/// END: concepts
///==================================================================================================================///
} // namespace helper
