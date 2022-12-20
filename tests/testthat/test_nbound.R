# File tests the n_bounds for connectivity and homology in 2 and 3 dimensions

test_that(
  'bound errors',{
    expect_error(n_bound_connect_2D(alpha=-1, delta=0.5))
    expect_error(n_bound_connect_2D(alpha=0.1, delta=-0.5))
    expect_error(n_bound_connect_3D(alpha=-1, delta=0.5))
    expect_error(n_bound_connect_3D(alpha=0.1, delta=-0.5))
    expect_error(n_bound_homology_2D(area=-1, epsilon=0.1))
    expect_error(n_bound_homology_2D(area=1, epsilon=-0.1))
    expect_error(n_bound_homology_2D(area=1, epsilon=0.1, tau = -1))
    expect_error(n_bound_homology_2D(area=1, epsilon=0.1, tau = 0.1))
    expect_error(n_bound_homology_2D(area=1, epsilon=0.1, delta=-5))
    expect_error(n_bound_homology_3D(volume=-1, epsilon=0.1))
    expect_error(n_bound_homology_3D(volume=1, epsilon=-0.1))
    expect_error(n_bound_homology_3D(volume=1, epsilon=0.1, tau = -1))
    expect_error(n_bound_homology_3D(volume=1, epsilon=0.1, tau = 0.1))
    expect_error(n_bound_homology_3D(volume=1, epsilon=0.1, delta=-5))
  }
)

test_that(
  'connect 2D',{
    #given
    my_alpha=0.2
    #when
    test = log(0.05)/log(1 - (pi*(2*my_alpha)^2/4))
    #then
    expect_equal(n_bound_connect_2D(my_alpha), ceiling(test)+1)
  }
)


test_that(
  'connect 3D',{
    #given
    my_alpha=0.2
    #when
    test = log(0.05)/log(1 - (pi*(2*my_alpha)^3/6))
    #then
    expect_equal(n_bound_connect_3D(my_alpha), ceiling(test)+1)
  }
)


test_that(
  'homology 2D',{
    #given
    area = 1
    epsilon = 0.01
    #when
    theta_1 = asin(epsilon/(8))
    theta_2 = asin(epsilon/(16))
    beta_1 = area/(cos(theta_1)^2 * (pi*(epsilon/4)^2))
    beta_2 = area/(cos(theta_2)^2 * (pi*(epsilon/8)^2))
    n_bound = beta_1*(log(beta_2)+log(20))
    #then
    expect_equal(n_bound_homology_2D(area, epsilon), ceiling(n_bound))
  }
)


test_that(
  'homolgy 3D',{
    #given
    volume=1
    epsilon = 0.01
    #when
    theta_1 = asin(epsilon/(8))
    theta_2 = asin(epsilon/(16))
    beta_1 = volume/(cos(theta_1)^3 * ((4/3)*pi*(epsilon/4)^3))
    beta_2 = volume/(cos(theta_2)^3 * ((4/3)*pi*(epsilon/8)^3))
    n_bound = beta_1*(log(beta_2)+log(20))
    #then
    expect_equal(n_bound_homology_3D(volume, epsilon), n_bound)
  }
)
