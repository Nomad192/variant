#include "details.h"

bad_variant_access::bad_variant_access() noexcept = default;
  
const char* bad_variant_access::what() const noexcept {
  return "bad_variant_access";
}