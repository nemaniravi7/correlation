!  correlation.f90 
!
!  FUNCTIONS:
!  correlation - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: correlation
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program correlation
    implicit none
    
    real :: x(4), y(4), i, n, sum_x, sum_y, x_bar, y_bar, num_xy, sq_x, sq_y, sap_xy, den_xy, cc
    
    !Enter and read the values of X
    print *, "Enter the values of X: "
    read *, x
    
    !Enter and read the values of Y
    print *, "Enter the values of Y: "
    read *, y
    
    sum_x = 0
    sum_y = 0   
    
    !Do loop for the sum of the values
    do i=1,4
        sum_x = sum_x + x(i)
        sum_y = sum_y + y(i)
    end do
  
    ! Displaying the values of the sums
    print*, "The sum of values in X is: ", sum_x
    print*, "The sum of values in Y is: ", sum_y
    
    !write(*,*) sum_x, sum_y
    x_bar = sum_x / 4
    y_bar = sum_y / 4
    
    ! Displaying the mean values
    print*, "The mean value of X is: ", x_bar
     print*, "The mean value of Y is: ", y_bar
  
     
      num_xy = 0
      
    !Do loop for numerator
    do i=1,4
        num_xy = num_xy + (x(i) - x_bar)*(y(i) - y_bar)
    end do

    ! Displaying the numerator 
    print*, "The NUMERATOR is: ", num_xy

   sq_x =0
   sq_y = 0
   
    ! DO loop for denominator
    do i=1,4
       sq_x = sq_x + ((x(i) - x_bar)**2)
       sq_y = sq_y + ((y(i) - y_bar)**2)
    end do

     sap_xy = sq_x * sq_y
    
     !Display of the under root value before root
     print*, "The sum and product of X and Y: ", sap_xy

    ! Net value of denominator
    den_xy = SQRT(sap_xy)
    print*, "The DENOMINATOR is: ", den_xy

    !Correlation coefficient formula which is NUMERATOR divided by DENOMINATOR
    cc = num_xy / den_xy

    !Final print of the value 
    print*, "The Correlation Coefficient is: ", cc 

    !End of the program Correlation_Coefficient
    
    end program correlation

