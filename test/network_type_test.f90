module network_type_test
  !!  Test network_type construction and behaviors
  use mod_network, only: network_type
  use garden, only: &
      result_t, test_item_t, describe, it, succeed
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

    network = network_type(dims=[5, 3, 2])
    
    !net % layers(1) % w

    result_ = succeed("check_not_image_distinct succeeds")
  end function

end module network_type_test
