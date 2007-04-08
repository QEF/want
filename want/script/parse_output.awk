#! /bin/awk -f

#
# parse_wannier.awk -- extract sensible information from the
#                      output file of wannier.x code
# 

BEGIN{ converged=0; end_of_iterations=0; program="" }

{ 
   #
   # first find the program which wrote the output
   #
   if ( NR < 15 ) 
   { 
      if ( match($0, "Program <wannier>") ) {
         program="wannier";
      } else if ( match($0, "Program <disentangle>") ) {
         program="disentangle";
      }
   }

   #
   # select the suitable parsing to do
   #
   if ( program == "wannier" ) 
   {
       parse_wannier();
   }
   else if ( program == "disentangle" )
   {
       parse_disentangle();
   }

}

END{ 
   if ( converged ) {
       print "STATUS@CONVERGED";
   } else if ( end_of_iterations ) { 
       print "STATUS@END_OF_ITER";
   } else {
       print "STATUS@UNKNOWN";
   }
}

function parse_wannier()
{
   #
   # first, check whether 
   # the calculation is converged or not
   #
   if ( match($0, "Max number of iteration reached") ) {
      end_of_iterations=1;
   }
   if ( match($0, "Convergence Achieved") ) {
      converged=1;
   }
   
   #
   # now, perform full check
   #
   if ( end_of_iterations || converged ) {
      check_line_wannier();
   }
}

function check_line_wannier()
{
   #
   # for each "sensible" value found, 
   # print KEYWORK @ value @ tollerance   (without blanks)
   #
   if ( match($0, "Iteration # :") ) 
      {
         print "ITERATION@"$4"@2";
      }
   else if ( match($0, "! Center Sum =") )
      {
         print "CENTER_SUM@"$NF"@1e-5";
      }
   else if ( match($0, "Omega I       =") )
      {
         print "OMEGA_I@"$NF"@1e-5";
      }
   else if ( match($0, "Omega D       =") )
      {
         print "OMEGA_D@"$NF"@1e-5";
      }
   else if ( match($0, "Omega OD      =") )
      {
         print "OMEGA_OD@"$NF"@1e-5";
      }
   else if ( match($0, "Omega Tot     =") )
      {
         print "OMEGA_TOT@"$NF"@1e-5";
      }
   else if ( match($0, "Omega Avrg    =") )
      {
         print "OMEGA_AVRG@"$NF"@1e-5";
      }
   else if ( match($0, "0   0   0     0.000000") )
      {
         print "ONSITE_HR@"$NF"@1e-5";
   }
}
   

function parse_disentangle()
{
   #
   # first, check whether 
   # the calculation is converged or not
   #
   if ( match($0, "Max number of iteration reached") ) {
      end_of_iterations=1;
   }
   if ( match($0, "Convergence Achieved") ) {
      converged=1;
   }
   
   #
   # now, perform full check
   #
   if ( end_of_iterations || converged ) {
      check_line_disentangle();
   }
}

function check_line_disentangle()
{
   #
   # for each "sensible" value found, 
   # print KEYWORK=value
   #
   if ( match($0, "Iteration # :") ) 
      {
         print "ITERATION@"$4"@2";
      }
   else if ( match($0, "Final Omega_I ") )
      {
         print "OMEGA_I@"$(NF-1)"@1e-5";
      }
   else if ( match($0, " Avrg Omega_I *:") )
      {
         print "OMEGA_I_AVRG@"$(NF-1)"@1e-5";
   }
}

