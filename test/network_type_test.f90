module network_type_test
  !!  Test network_type construction and behaviors
  use mod_network, only: network_type
  use mod_kinds, only: rk
  use user_defined_collectives_m, only : co_all
  use garden, only: &
      result_t, test_item_t, describe, it, assert_that
  implicit none
  private
  public :: test_network_construction

contains

  function test_network_construction() result(tests)
    type(test_item_t) :: tests
    type(network_type) :: net

    tests = describe("A network_type object", &
      [ it("is identical across images after construction", check_for_identical_networks) &
      ])

  end function

  function check_for_identical_networks() result(result_)
    type(result_t) result_
    type(network_type) network
    real(rk), allocatable :: expected_weights(:,:)
    integer :: expected_shape(2)
    logical :: weights_allocated, shapes_match, weights_match
    integer, parameter :: broadcaster=1

    network = network_type(dims=[5, 3, 2])
 
    weights_allocated = allocated(network%layers(1)%w)
    call co_all(weights_allocated)

    check_allocation: &
    if (.not. weights_allocated) then
      shapes_match = .false.
      weights_match = .false.
    else 
      associate(weights => network%layers(1)%w)
        if (this_image()==broadcaster) expected_shape = shape(weights)
        call co_broadcast(expected_shape, source_image=broadcaster)
        shapes_match = all(shape(weights) == expected_shape)
        call co_all(shapes_match)

        check_shape: &
        if (.not. shapes_match ) then
          weights_match = .false.
        else 
          if (this_image()==broadcaster) then
            expected_weights = weights
          else
            allocate(expected_weights, mold = weights)
          end if
          call co_broadcast(expected_weights, source_image=broadcaster)
          weights_match = all(weights == expected_weights)
          call co_all(weights_match)
        end if check_shape
      end associate
    end if check_allocation

    result_ = assert_that(weights_allocated, "all weights allocated") .and. &
              assert_that(shapes_match, "all weights array shapes match across images") .and. &
              assert_that(weights_match, "all weights match across images")
  end function

end module network_type_test
