module network_type_test
  !!  Test network_type constructor and type-bound procedures.
  use mod_network, only: network_type
  use garden, only: &
      result_t, test_item_t, describe, it, succeed
  implicit none
  private
  public :: test_network_type

contains

  function test_network_type() result(tests)
    type(test_item_t) :: tests

    tests = describe("A network_type object", &
      [ it("is identical across images after construction", check_identical_networks) &
      ])
  end function

  function check_identical_networks() result(result_)
    type(result_t) result_
    type(network_type) network
    integer i

    network = network_type(dims=[5, 3, 2])
    do i=1, size(network%layers)
      print *,this_image(),",",i,":",network%layers(i)%w
    end do
    result_ = succeed("")
  end function

end module network_type_test
