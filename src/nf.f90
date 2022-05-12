module nf
  use nf_datasets_mnist, only: label_digits, load_mnist
  use nf_layer, only: layer
  use nf_layer_constructors, only: conv2d, dense, input
  use nf_network, only: network
end module nf
