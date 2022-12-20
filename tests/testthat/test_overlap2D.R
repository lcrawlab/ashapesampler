# testing overlap functions for 2D. Circle, square, annulus.
test_that(
  'Overlap Errors',{
    expect_error(calc_overlap_2D(alpha=-1))
    expect_error(calc_overlap_2D(alpha=1, r=-1))
    expect_error(calc_overlap_2D(alpha=1, r=0.5, rmin=-1))
    expect_error(calc_overlap_2D(alpha=1, r=0.5, rmin=1))
    expect_error(calc_overlap_2D(alpha=1, bound = "triangle"))
    expect_error(circle_overlap_ia(3.4, 2, 1))
    expect_error(calc_overlap_2D(alpha=1, r=1, rmin=2, bound="annulus"))
  }
)

test_that(
  'Overlap Square', {
    #given
    rad=0.5
    alpha1=0.2
    alpha2= 0.35
    alpha3 = 1
    theta=acos(0.5/0.7)
    a1 = 0.5*(theta-sin(theta))*0.7^2
    #when
    overlap1 = calc_overlap_2D(alpha=alpha1, r=rad)
    overlap2 = calc_overlap_2D(alpha=alpha2, r=rad)
    overlap3 = calc_overlap_2D(alpha=alpha3, r=rad)
    #then
    expect_equal(overlap1, (pi*0.4^2/4)/0.25)
    expect_equal(overlap2, (pi*0.7^2/4 - a1)/0.25)
    expect_equal(overlap3, 1)
  }
)

test_that(
  'Overlap Circle', {
    #given
    rad=0.5
    alpha1 = 0.25
    alpha2 = 0.4
    alpha3 = 2
    theta1 = 2*acos(0.5)
    theta2 = 2*acos(0.5)
    a1 = 0.5*(theta1-sin(theta1))*0.5^2
    a2 = 0.5*(theta2-sin(theta2))*0.5^2
    o1 = a1+a2
    theta1 = 2*acos(0.8)
    theta2 = 2*acos(-0.28)
    a1 = 0.5*(theta1-sin(theta1))*0.8^2
    a2 = 0.5*(theta2 + sin((2 * pi - theta2)))*0.5^2
    o2 = a1+a2
    #when
    overlap1 = calc_overlap_2D(alpha=alpha1, r=rad, bound="circle")
    overlap2 = calc_overlap_2D(alpha=alpha2, r=rad, bound="circle")
    overlap3 = calc_overlap_2D(alpha=alpha3, r=rad, bound="circle")
    #then
    expect_equal(overlap1, o1/(pi*rad^2))
    expect_equal(overlap2, o2/(pi*rad^2))
    expect_equal(overlap3, 1)
  }
)

test_that(
  'Overlap Annulus', {
  #given
  R_big = 2
  r_lit = 1
  total = pi*3
  #First overlap
  alpha1 = 0.25 # 0 < 0.5 < 1
  alpha2 = 0.75 # 1 < 1.5 < 3
  alpha3 = 1.7  # 3 < 3.4 < 4
  alpha4 = 3    # 4 < 6
  theta1 = 2*acos(0.125)
  theta2 = 2*acos(0.96875)
  a1 = 0.5*(theta1-sin(theta1))*0.5^2
  a2 = 0.5*(theta2-sin(theta2))*2^2
  o1 = a1+a2
  #Second overlap
  theta1 = 2*acos(0.375)
  theta2 = 2*acos(0.71875)
  theta3 = 2*acos(((2*alpha2)^2+R_big^2-r_lit^2)/(2*(2*alpha2)*R_big))
  theta4 = 2*acos((R_big^2+r_lit^2-(2*alpha2)^2)/(2*r_lit*R_big))
  a1 = 0.5*(theta1-sin(theta1))*1.5^2
  a2=0
  if (theta2 < pi){
    a2 = R_big^2*0.5*(theta2 - sin(theta2))
  } else {
    a2 = R_big^2*0.5*(theta2 + sin((2*pi-theta2)))
  }
  a3 = 1.5^2*0.5*(theta3 - sin(theta3))
  a4 = 0
  if (theta4< pi){
    a4 = r_lit^2*0.5*(theta4 - sin(theta4))
  } else {
    a4 = r_lit^2*0.5*(theta4 + sin((2*pi-theta4)))
  }
  o2 = a1+a2-a3-a4
  #Third overlap
  theta1 = 2*acos(0.85)
  theta2 = 2*acos(-0.445)
  a1 = 0.5*(theta1-sin(theta1))*(2*alpha3)^2
  a2=0 
  if (alpha3*2 < (R_big*sqrt(2))){
    a2 = R_big^2 * 0.5 * (theta2 - sin(theta2))
  } else {
    a2 = R_big^2 * 0.5 * (theta2 + sin((2 * pi - theta2)))
  }
  o3 = a1+a2
  #when
  overlap1 = calc_overlap_2D(alpha=alpha1, r=R_big, rmin=r_lit, bound="annulus")
  overlap2 = calc_overlap_2D(alpha=alpha2, r=R_big, rmin=r_lit, bound="annulus")
  overlap3 = calc_overlap_2D(alpha=alpha3, r=R_big, rmin=r_lit, bound="annulus")
  overlap4 = calc_overlap_2D(alpha=alpha4, r=R_big, rmin=r_lit, bound="annulus")
  #then
  expect_equal(overlap1, o1/total)
  expect_equal(overlap2, o2/total)
  expect_equal(overlap3, (o3 - pi*r_lit^2)/total)
  expect_equal(overlap4, 1)
  }
)