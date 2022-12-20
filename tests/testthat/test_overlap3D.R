# testing overlap functions for 3D. Sphere, cube, shell.
test_that(
  'Overlap Errors',{
    expect_error(calc_overlap_3D(alpha=-1))
    expect_error(calc_overlap_3D(alpha=1, r=-1))
    expect_error(calc_overlap_3D(alpha=1, r=0.5, rmin=-1))
    expect_error(calc_overlap_3D(alpha=1, r=0.5, rmin=1))
    expect_error(calc_overlap_3D(alpha=1, bound = "triangle"))
    expect_error(sphere_overlap_is(3.4, 2, 1))
  }
)

test_that(
  'Overlap Cube', {
    #given
    rad=0.5
    alpha1=0.2      #0<0.4<0.5
    alpha2= 0.35    #0.5< 0.7 < sqrt(2)/2
    alpha3 = 0.4    #sqrt(2)/2 < 0.8 < sqrt(3)/2
    alpha4 = 1
    vc2 = pi*(2*alpha2-rad)^2*(4*alpha2+rad)/3
    vc3 = pi*(2*alpha3-rad)^2*(4*alpha3+rad)/3
    vo = cap_intersect_vol(2*alpha3, rad)
    #when
    overlap1 = calc_overlap_3D(alpha=alpha1, r=rad)
    overlap2 = calc_overlap_3D(alpha=alpha2, r=rad)
    overlap3 = calc_overlap_3D(alpha=alpha3, r=rad)
    overlap4 = calc_overlap_3D(alpha=alpha4, r=rad)
    #then
    expect_equal(overlap1, (pi*(2*alpha1)^3/6)/0.5^3)
    expect_equal(overlap2, (pi*(2*alpha2)^3/6 - 0.75*vc2)/0.5^3)
    expect_equal(overlap3, (pi*(2*alpha3)^3/6-3*(0.25*vc3-vo))/0.5^3)
    expect_equal(overlap4, 1)
  }
)

test_that(
  'Overlap Sphere', {
    #given
    rad=0.5
    alpha1 = 0.25
    alpha2 = 0.4
    alpha3 = 2
    total = (4/3)*pi*0.5^3
    theta1 = acos(0.5)
    h2 = (2*alpha1)*cos(theta1)
    h1 = 2*alpha1-h2
    v1 = (1/3)*pi*h1^2*(3*2*alpha1 - h1)
    v2 = (1/3)*pi*h2^2*(3*rad-h2)
    o1 = v1+v2
    theta1 = acos(0.8)
    h2 = (2*alpha2)*cos(theta1)
    h1 = 2*alpha2-h2
    v1 = (1/3)*pi*h1^2*(3*2*alpha2 - h1)
    v2 = (1/3)*pi*h2^2*(3*rad-h2)
    if (h2>rad){
      h = 2*rad-h2
      v2 = (4/3)*pi*rad^3 - (1/3)*pi*h^2*(3*rad-h)
    }
    o2 = v1+v2
    #when
    overlap1 = calc_overlap_3D(alpha=alpha1, r=rad, bound="sphere")
    overlap2 = calc_overlap_3D(alpha=alpha2, r=rad, bound="sphere")
    overlap3 = calc_overlap_3D(alpha=alpha3, r=rad, bound="sphere")
    #then
    expect_equal(overlap1, o1/total)
    expect_equal(overlap2, o2/total)
    expect_equal(overlap3, 1)
  }
)

test_that(
  'Overlap Shell', {
    #given
    R_big = 2
    r_lit = 1
    total = (4/3)*pi*(R_big^3-r_lit^3)
    #First overlap
    alpha1 = 0.25 # 0 < 0.5 < 1
    alpha2 = 0.75 # 1 < 1.5 < 3
    alpha3 = 1.7  # 3 < 3.4 < 4
    alpha4 = 3    # 4 < 6
    theta1 = acos(0.125)
    h2 = (2*alpha1)*cos(theta1)
    h1 = 2*alpha1-h2
    v1 = (1/3)*pi*h1^2*(3*2*alpha1 - h1)
    v2 = (1/3)*pi*h2^2*(3*R_big-h2)
    o1 = v1+v2
    #Second overlap
    theta1 = acos(0.375)
    theta3 = acos(((2*alpha2)^2+R_big^2-r_lit^2)/(2*(2*alpha2)*R_big))
    h2 = (2*alpha2)*cos(theta1)
    h1 = 2*alpha2-h2
    v1 = (1/3)*pi*h1^2*(3*2*alpha2 - h1)
    v2 = (1/3)*pi*h2^2*(3*R_big-h2)
    h4 = (2*alpha2)*cos(theta3)
    h3 = 2*alpha2-h4
    v3 = (1/3)*pi*h3^2*(3*2*alpha2-h3)
    v4 = (1/3)*pi*h4^2*(3*r_lit-h4)
    o2 = v1+v2-v3-v4
    #Third overlap
    theta1 = acos(0.85)
    theta2 = acos(-0.445)
    h2 = 2*alpha3*cos(theta1)
    h1 = 2*alpha3-h2
    v1 = (1/3)*pi*h1^2*(3*2*alpha3 - h1)
    v2 = 0
    if (h2 <= R_big){
      v2 = (1/3)*pi*h2^2*(3*R_big-h2)
    } else {
      h2 = 2*R_big-h2
      v2 = (4/3)*pi*R_big^3 - (1/3)*pi*h2^2*(3*R_big-h2)
    }
    o3 = v1+v2
    #when
    overlap1 = calc_overlap_3D(alpha=alpha1, r=R_big, rmin=r_lit, bound="shell")
    overlap2 = calc_overlap_3D(alpha=alpha2, r=R_big, rmin=r_lit, bound="shell")
    overlap3 = calc_overlap_3D(alpha=alpha3, r=R_big, rmin=r_lit, bound="shell")
    overlap4 = calc_overlap_3D(alpha=alpha4, r=R_big, rmin=r_lit, bound="shell")
    #then
    expect_equal(overlap1, o1/total)
    expect_equal(overlap2, o2/total)
    expect_equal(overlap3, (o3 - (4/3)*pi*r_lit^3)/total)
    expect_equal(overlap4, 1)
  }
)