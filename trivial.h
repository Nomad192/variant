#pragma once

#include <new>
#include <utility>

// struct nullopt_t {};
// inline constexpr nullopt_t nullopt{};
//
// struct in_place_t {};
// inline constexpr in_place_t in_place{};

///==================================================================================================================///

//template <typename... Types>
//union Multi_Union {};
//
//template <typename First, typename... Rest_Types>
//union Multi_Union<First, Rest_Types...> {
//  //constexpr Multi_Union() = default;
//  //  constexpr Multi_Union() : rest() { }
//  //
//  //  template<typename... _Args>
//  //  constexpr Multi_Union(in_place_index_t<0>, _Args&&... __args)
//  //      : first(in_place_index<0>, std::forward<_Args>(__args)...)
//  //  { }
//  //
//  //  template<size_t _Np, typename... _Args>
//  //  constexpr _Variadic_union(in_place_index_t<_Np>, _Args&&... __args)
//  //      : _M_rest(in_place_index<_Np-1>, std::forward<_Args>(__args)...)
//  //  { }
//
//  First first;
//  Multi_Union<Rest_Types...> rest;
//};

///==================================================================================================================///

template<bool trivial, typename... Types>
struct base_ {};

template <typename First, typename... Rest_Types>
struct base_<false, First, Rest_Types...> {
  base_() {}

  //  template <typename... Args>
  //  constexpr explicit base(in_place_t, Args&&... args)
  //      : is_present{true}, data(std::forward<Args>(args)...) {}

  //Multi_Union<Types...> data;

  //  bool is_present{false};
  //  union {
  //    char dummy{};
  //    T data;
  //  };
  //
  //  constexpr void reset() {
  //    if (is_present) {
  //      data.~T();
  //    }
  //    is_present = false;
  //  }
  //
//    ~base_() {
//      //reset();
//    }

  First first;
  base_<std::conjunction_v<std::is_trivially_destructible<Rest_Types>...>, Rest_Types...> rest;
};

template <typename First, typename... Rest_Types>
struct base_<true, First, Rest_Types...> {
  base_() {}

  //Multi_Union<Rest_Types...> data;

  First first;
  base_<std::conjunction_v<std::is_trivially_destructible<Rest_Types>...>, Rest_Types...> rest;
  //  template <typename... Args>
  //  constexpr explicit base(in_place_t, Args&&... args)
  //      : is_present{true}, data(std::forward<Args>(args)...) {}
  //
  //  bool is_present{false};
  //  union {
  //    char dummy{};
  //    T data;
  //  };
  //
  //  constexpr void reset() {
  //    is_present = false;
  //  }
};

template <typename... Types>
using base = base_<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

///==================================================================================================================///

// template <typename T, bool trivial = std::is_trivially_copy_constructible_v<T>>
// struct trivial_copy_constructable_base : base<T> {
template <bool trivial = false, typename... Types>
struct trivial_copy_constructable_base_ : base<Types...> {
  using base<Types...>::base;
  constexpr trivial_copy_constructable_base_(const trivial_copy_constructable_base_& other) : base<Types...>() { ///???
    //    if (other.is_present) {
    //      new (&this->data) T(other.data);
    //      this->is_present = true;
    //    }
  }
  constexpr trivial_copy_constructable_base_(trivial_copy_constructable_base_&&) = default;
};

template <typename... Types>
struct trivial_copy_constructable_base_<true, Types...> : base<Types...> {
  using base<Types...>::base;
};

template <typename... Types>
using trivial_copy_constructable_base =
    trivial_copy_constructable_base_<std::conjunction_v<std::is_trivially_copy_constructible<Types>...>, Types...>;

///==================================================================================================================///

// template <typename T, bool trivial = std::is_trivially_copy_constructible_v<T>&&
//                           std::is_trivially_copy_assignable_v<T>>
// struct trivial_copy_assign_base : trivial_copy_constructable_base<T> {
//   using trivial_copy_constructable_base<T>::trivial_copy_constructable_base;
template <bool trivial = false, typename... Types>
struct trivial_copy_assign_base_ : trivial_copy_constructable_base<Types...> {
  using trivial_copy_constructable_base<Types...>::trivial_copy_constructable_base;
  constexpr trivial_copy_assign_base_(const trivial_copy_assign_base_& other) = default;
  trivial_copy_assign_base_& operator=(const trivial_copy_assign_base_& other) {
    //    if (this->is_present) {
    //      if (other.is_present) {
    //        this->data = other.data;
    //      } else {
    //        this->reset();
    //      }
    //    } else {...
    //      if (other.is_present) {
    //        new (&this->data) T(other.data);
    //        this->is_present = true;
    //      }
    //    }

    return *this;
  }
};

template <typename... Types>
struct trivial_copy_assign_base_<true, Types...> : trivial_copy_constructable_base<Types...> {
  using trivial_copy_constructable_base<Types...>::trivial_copy_constructable_base;
};

template <typename... Types>
using trivial_copy_assign_base =
    trivial_copy_assign_base_<std::conjunction_v<std::is_trivially_copy_constructible<Types>...> &&
                                  std::conjunction_v<std::is_trivially_copy_assignable<Types>...>,
                              Types...>;

///==================================================================================================================///

// template <typename T, bool trivial = std::is_trivially_move_constructible_v<T>>
// struct trivial_move_constructable_base : trivial_copy_assign_base<T> {
//   using trivial_copy_assign_base<T>::trivial_copy_assign_base;
template <bool trivial = false, typename... Types>
struct trivial_move_constructable_base_ : trivial_copy_assign_base<Types...> {
  using trivial_copy_assign_base<Types...>::trivial_copy_assign_base;
  constexpr trivial_move_constructable_base_(const trivial_move_constructable_base_&) = default;
  constexpr trivial_move_constructable_base_(trivial_move_constructable_base_&& other) {
//    if (other.is_present) {
//      new (&this->data) T(std::move(other.data));
//      this->is_present = true;
//    }
  }
  trivial_move_constructable_base_& operator=(const trivial_move_constructable_base_&) = default;
};

template <typename... Types>
struct trivial_move_constructable_base_<true, Types...> : trivial_copy_assign_base<Types...> {
  using trivial_copy_assign_base<Types...>::trivial_copy_assign_base;
};

template <typename... Types>
using trivial_move_constructable_base =
    trivial_move_constructable_base_<std::conjunction_v<std::is_trivially_move_constructible<Types>...>, Types...>;

///==================================================================================================================///

//template <typename T, bool trivial = std::is_trivially_move_constructible_v<T>&& std::is_trivially_move_assignable_v<T>>
//struct trivial_move_assign_base : trivial_move_constructable_base<T> {
//  using trivial_move_constructable_base<T>::trivial_move_constructable_base;
template <bool trivial = false, typename... Types>
struct trivial_move_assign_base_ : trivial_move_constructable_base<Types...> {
  using trivial_move_constructable_base<Types...>::trivial_move_constructable_base;
  constexpr trivial_move_assign_base_(const trivial_move_assign_base_&) = default;
  constexpr trivial_move_assign_base_(trivial_move_assign_base_&& other) = default;

  trivial_move_assign_base_& operator=(const trivial_move_assign_base_&) = default;
  trivial_move_assign_base_& operator=(trivial_move_assign_base_&& other) {
//    if (this->is_present) {
//      if (other.is_present) {
//        this->data = std::move(other.data);
//      } else {
//        this->reset();
//      }
//    } else {
//      if (other.is_present) {
//        new (&this->data) T(std::move(other.data));
//        this->is_present = true;
//      }
//    }
    return *this;
  }
};

template <typename... Types>
struct trivial_move_assign_base_<true, Types...> : trivial_move_constructable_base<Types...> {
  using trivial_move_constructable_base<Types...>::trivial_move_constructable_base;
};

template <typename... Types>
using trivial_move_assign_base =
    trivial_move_assign_base_<std::conjunction_v<std::is_trivially_move_constructible<Types>...> &&
                                  std::conjunction_v<std::is_trivially_move_assignable<Types>...>,
                              Types...>;
