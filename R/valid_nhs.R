#' @title NHS Number Validity Check
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Validates NHS numbers using the **NHS checksum** applied to the
#' first 9 digits and compared with the 10th digit. Inputs that are
#' not exactly **10 numeric characters** are invalid. Numbers made of the **same
#' repeated digit** (e.g., `"1111111111"`, `"0000000000"`) are also rejected.
#'
#' @details
#' **Algorithm (summary):**
#' 1. Take the first 9 digits and multiply by weights **10 down to 2** (i.e.,
#'    d1×10 + d2×9 + … + d9×2).
#' 2. Compute `11 - (sum %% 11)` → this is the **expected check digit**.
#' 3. If the expected check digit is **11**, treat as **0**.
#' 4. Compare with the **10th digit**. If they match, the number is valid.
#'
#' Additional guards implemented:
#' - If the NHS number is `NA` or not **10 characters**, it is **invalid**.
#' - If **all 10 digits are identical** (e.g., `"1111111111"`), it is **invalid**.
#'
#' The function is vectorised and returns **1 for valid** and **0 for invalid**
#' for each element in the input vector.
#'
#' @param nhs_number A vector of values to validate. Each element is coerced to
#'   character for length checking and digit extraction.
#'
#' @examples
#' test <- floor(runif(1000,1000000000,9999999999))
#' valid_nhs(test)
#' valid_nhs(9434765919)
#'
#' @return A numeric vector of the same length as `nhs_number` containing:
#'   - `1` if the value is a valid NHS number
#'   - `0` otherwise
#'
#' @export

valid_nhs <- function(nhs_number){

  ## create a nested function
  ## if(is.na(x)) uses the first element of a vector
  ## sapply of the nested function applies the function to each element

  checksum_algorithm <- function(NHS){

    ## immediately fail the missing NHS numbers OR
    ## if its got the wrong number of digits

    if(is.na(NHS) || NHS == ""){

      warning("NHS number is missing or empty")

      ReturnValue <- 0

    } else if(nchar(as.character(NHS))!=10) {

      warning(paste(
        "Invalid NHS number length:",
        NHS,
        "- must be exactly 10 digits"
      ))

      ReturnValue <- 0

    } else {

      ## break up the NHS number into its individual digits
      n1 <- as.numeric(substr(NHS, 1, 1))
      n2 <- as.numeric(substr(NHS, 2, 2))
      n3 <- as.numeric(substr(NHS, 3, 3))
      n4 <- as.numeric(substr(NHS, 4, 4))
      n5 <- as.numeric(substr(NHS, 5, 5))
      n6 <- as.numeric(substr(NHS, 6, 6))
      n7 <- as.numeric(substr(NHS, 7, 7))
      n8 <- as.numeric(substr(NHS, 8, 8))
      n9 <- as.numeric(substr(NHS, 9, 9))
      n10 <- as.numeric(substr(NHS, 10, 10))

      ## are all the numbers the same
      UniformNumberCheck <- ifelse((n1 == n2) &
                                     (n2 == n3) &
                                     (n3 == n4) &
                                     (n4 == n5) &
                                     (n5 == n6) &
                                     (n6 == n7) &
                                     (n7 == n8) &
                                     (n8 == n9) &
                                     (n9 == n10),
                                   1,
                                   0)

      ## what is the remainder
      Modulus <- (
        (n1 * 10) +
          (n2 * 9) +
          (n3 * 8) +
          (n4 * 7) +
          (n5 * 6) +
          (n6 * 5) +
          (n7 * 4) +
          (n8 * 3) +
          (n9 * 2)
      )

      Modulus <- (11 - (Modulus %% 11))

      ## 1 is valid, 0 is not
      ReturnValue <-
        ifelse(
            UniformNumberCheck != 1 &
            (Modulus == n10 | (Modulus == 11 & n10 == 0)),
          1, 0)

      }

    return(ReturnValue)

  }

  ## to prevent NA errors when looking at a vector of NHS numbers
  purrr::map_dbl(nhs_number,checksum_algorithm)

}
