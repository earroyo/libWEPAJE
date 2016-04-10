README:

libWEPAJE: Contains a collection of function to solve the difusivity equation in the Laplace space. Solution for complex equations are readily available as a simple Excel function. Anyone can use the functions without the need of software license or specialized software application

Notice there is an Excel file and a .dll file. You need to copy both files. Once you place the Excel file and the .dll file in the directory of your preference, you will need to (in Excel 2007) click in the upper left corner the "big circle" choose "Excel Options" then go to "Popular" then click where it says: "Show Developer tab in Ribbon".
 
Your Excel should have now an additional menu that says "Developer", click on it, then click on first Icon where it says "Visual Basic". It should open a new window where you can see the code for the macros and other stuff. On the left hand side click where it says Modules -> libWEPAJEWrapers
 
Change the location of the path with the location of the libWEPAJE.dll file 
 
Declare Function QDNFBDLL _
Lib "C:\Users\arroyone\Desktop\libWEPAJE\libWEPAJE.dll" _
(ByVal t As Double, ByVal R As Double) As Double
 
Notice I have it pointing to "C:\Users\arroyone\Desktop\libWEPAJE\libWEPAJE.dll" change this line to the folder where you copied your file.
