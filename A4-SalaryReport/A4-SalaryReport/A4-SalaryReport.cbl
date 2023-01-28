       IDENTIFICATION DIVISION.
       PROGRAM-ID. A4.
       author. David Osagiede.
       date-written. 2022-03-1.
      *Description: The purpose of this program is to output 
      *the print-line information for the salary report regarding the
      *programmers using the input file

       environment division.
       input-output section.
       file-control.

            select input-file
                assign to '../../../data/A4.dat'
                organization is line sequential.

            select print-file
                assign to '../../../data/A4-SalaryReport.out'
                organization is line sequential.

       data division.
       file section.
       fd input-file
          record contains 28 characters
          data record is employee-record.

      * records for the information pertaining to the employees
       01 employee-record.
         05 er-employee-num           pic x(3).
         05 er-employee-name          pic x(15).
         05 er-education-code         pic x(1).
           88 er-grad                             value 'G'.
           88 er-non-grad                         value 'N'.
         05 er-years-service          pic 9(2).
      * classifications for employees
           88 er-years-16plus                     value 16 thru 99.
           88 er-years-7-15                       value 7  thru 15.
           88 er-years-3-6                        value 3  thru 6.
           88 er-years-11plus                     value 11 thru 99.
           88 er-years-5-10                       value 5  thru 10.

         05 er-current-salary         pic 9(5)V99.

       fd print-file
           record contains 90 characters
           data record is print-line.
      * print-line for important employee information
       01 print-line.
         05 filler pic x(2).
         05 pl-employee-num           pic x(3).
         05 filler                    pic x(2).
         05 pl-employee-name          pic x(15).
         05 filler                    pic x(2).
         05 pl-years-service          pic 9(2).
         05 filler                    pic x(5).
         05 pl-job-code               pic x(10).
         05 filler                    pic x(5).
         05 pl-current-salary         pic ZZ,ZZ9.99.
         05 filler                    pic x.
         05 pl-increase               pic ZZZ.Z.
         05 pl-increase-sign          pic x value '%'.
         05 filler                    pic x(3).
         05 pl-pay-increase           pic $$,$$9.99+.
         05 filler                    pic x.
         05 pl-new-salary             pic $Z,ZZZ,ZZ9.99.
         05 filler                    pic x.

       working-storage section.
      * multiple footers & headers for the output
       01 ws-report-header.
         05 filler                    pic x(28)   value 
                                    "David Osagiede, Assignment 4".
         05 filler                    pic x(14)   value spaces.
         05 filler                    pic x(8)    value "20210411".
         05 filler                    pic x(32)   value spaces.
         05 filler                    pic x(7)    value "1951043".
         05 filler                    pic x(3)    value spaces.
       01 ws-page-footer1.
         05 filler                    pic x(14)   value
                                                 "Employee Class".
         05 filler                    pic x(8)    value spaces.
         05 filler                    pic x(7)    value "Analyst".
         05 filler                    pic x(8)    value spaces.
         05 filler                    pic x(8)    value "Sen Prog".
         05 filler                    pic x(8)    value spaces.
         05 filler                    pic x(5)    value "Prog".
         05 filler                    pic x(7)    value spaces.
         05 filler                    pic x(8)    value "Jr Prog".
         05 filler                    pic x(4)    value spaces.
         05 filler                    pic x(13)   value "Unclassified".
       
       01 ws-page-footer2.
         05 filler                    pic x(16) value 
                                      "# On this page: ".
      * count for the different types of programmers
         05 filler                    pic x(11)   value spaces.
         05 ws-anal-count             pic Z9      value 0.
         05 filler                    pic x(14)   value spaces.
         05 ws-senior-prog-count      pic z9      value 0.
         05 filler                    pic x(10)   value spaces.
         05 ws-prog-count             pic z9      value 0.
         05 filler                    pic x(13)   value spaces.
         05 ws-jr-prog-count          pic z9      value 0.
         05 filler                    pic x(15)   value spaces.
         05 ws-unclassified-count     pic z9      value 0.
         05 filler                    pic x(1)    value spaces.
      * average amount for each classifications
       01 ws-report-footer1.
         05 filler                    pic x(21)   value 
                                            "Average Increases: ".
         05 filler                    pic x(10)   value "Analyst= ".
         05 ws-analyst-avg            pic ZZ,ZZ9.99
                                                  value 0.
         05 filler                    pic x(5)    value spaces.
         05 filler                    pic x(10)   value "Sen Prog= ".
         05 ws-sen-avg                pic ZZ,ZZ9.99
                                                  value 0.
         05 filler                    pic x(26)   value spaces.
       01 ws-report-footer2.
         05 filler                    pic x(21)   value spaces.
         05 filler                    pic x(10)   value "Prog= ".
         05 ws-prog-avg               pic ZZ,ZZ9.99                                              
                                                  value 0.
         05 filler                    pic x(5)    value spaces.
         05 filler                    pic x(10)   value "Jr Prog= ".
         05 ws-jr-prog-avg            pic ZZ,ZZ9.99
                                                  value 0.
         05 filler                    pic x(26)   value spaces.
       01 ws-page-header.
         05 filler                    pic x(30)   value spaces.
         05 filler                    pic x(22)   value 
                                        "EMPLOYEE SALARY REPORT".
         05 filler                    pic x(21)   value spaces.
         05 filler                    pic x(5)    value "PAGE ".
         05 ws-page-num               pic Z9      value 0.
         05 filler                    pic x(10)   value spaces.
       01 ws-eof                      pic x       value 'n'.

      * headings 
       01 ws-heading1.
         05 filler                    pic x(2)    value spaces.
         05 filler                    pic x(3)    value "EMP".
         05 filler                    pic x(3)    value spaces.
         05 filler                    pic x(3)    value "EMP".
         05 filler                    pic x(36)   value spaces.
         05 filler                    pic x(8)    value "Present".
         05 filler                    pic x(1)    value spaces.
         05 filler                    pic x(8)    value "Increase".
         05 filler                    pic x(6)    value spaces.
         05 filler                    pic x(4)    value "Pay".
         05 filler                    pic x(8)    value spaces.
         05 filler                    pic x(3)    value "New".
         05 filler                    pic x(5)    value spaces.
       01 ws-heading2.
         05 filler                    pic x(2)    value spaces.
         05 filler                    pic x(3)    value "NUM".
         05 filler                    pic x(3)    value spaces.
         05 filler                    pic x(5)    value "NAME".
         05 filler                    pic x(10)   value spaces.
         05 filler                    pic x(5)    value "Years".
         05 filler                    pic x(3)    value spaces.
         05 filler                    pic x(8)    value "Position".
         05 filler                    pic x(9)    value spaces.
         05 filler                    pic x(9)    value "Salary".
         05 filler                    pic x(2)    value spaces.
         05 filler                    pic x(1)    value "%".
         05 filler                    pic x(7)    value spaces.
         05 filler                    pic x(9)    value "Increase".
         05 filler                    pic x(5)    value spaces.
         05 filler                    pic x(6)    value "Salary".
         05 filler                    pic x(3) value spaces.
      * data used for the calculations for different classes
       77 cnst-percent-analyst        pic 99V9    value 12.8.
       77 cnst-percent-senior-analyst pic 99V9    value 9.3.
       77 cnst-percent-programmer     pic 99V9    value 6.7.
       77 cnst-percent-jr-prog        pic 99V9    value 3.2.
       77 cnst-percent-unclass        pic 99V9    value 0.

       77 ws-percent-increase         pic 99V9    value 0.
       77 ws-count-analyst            pic 99      value 0.
       77 ws-count-sen-prog           pic 99      value 0.
       77 ws-count-prog               pic 99      value 0.
       77 ws-count-jr-prog            pic 99      value 0.
       77 ws-count-unclassified       pic 99      value 0.
       77 ws-total-analyst            pic 99      value 0.
       77 ws-total-sen-prog           pic 99      value 0.
       77 ws-total-prog               pic 99      value 0.
       77 ws-total-jr-prog            pic 99      value 0.
       77 ws-total-unclassified       pic 99      value 0.
       77 ws-avg-analyst              pic 9(6)V99 value 0.
       77 ws-avg-sen-prog             pic 9(6)V99 value 0.
       77 ws-avg-prog                 pic 9(6)V99 value 0.
       77 ws-avg-jr-prog              pic 9(6)V99 value 0.
       77 ws-avg-unclassified         pic 9(6)V99 value 0.
       77 ws-lines-per-page           pic 99      value 10.
       77 ws-page-count               pic 99      value 0.
       77 ws-line-count               pic 99      value 0.
       77 ws-column-headings          pic 9(10).

       procedure division.

       000-Main.
      *opens  + reads input file & gets output from the print file
                     open input input-file,
                         output print-file.

                              read input-file
                                 at end
                                    move "y" to ws-eof.
      * Report header

           write print-line from ws-report-header before advancing 2
             lines.

           perform 10-process-pages until ws-eof = "y".


      * calculate the averages for the different programmers
        compute ws-avg-analyst  rounded = ws-avg-analyst /
             ws-total-analyst.
        compute ws-avg-sen-prog rounded = ws-avg-sen-prog /
             ws-total-sen-prog.
        compute ws-avg-jr-prog  rounded = ws-avg-jr-prog /
             ws-total-jr-prog.
        compute ws-avg-prog     rounded = ws-avg-prog / ws-total-prog.
           move ws-avg-analyst               to ws-analyst-avg.
           move ws-avg-jr-prog               to ws-jr-prog-avg.
           move ws-avg-sen-prog              to ws-sen-avg.
           move ws-avg-prog                  to ws-prog-avg.
      * prints the report footer 2 & advances two lines
           write print-line    from ws-report-footer1 after advancing 2
             line.
      * prints report footer 2
           write print-line    from ws-report-footer2.
    

           close print-file,
             input-file.

           stop run.

      * closes print file and stops running
      *
           goback.

       10-process-pages.
      *
      * counting this page and printing headings page
      *
           add 1                             to ws-page-count.
           move ws-page-count                to ws-page-num.
           move spaces                       to print-line.

      *
      *
      *       and need to advance page for all but the first page
      *
           if (ws-page-count > 1) then
               write print-line from ws-page-header
                 after advancing page
           else
               write print-line from ws-page-header
           end-if.
      * Columns headings
           write print-line     from ws-heading1
             after advancing 2 lines.
           write print-line     from ws-heading2.
           add ws-count-analyst              to ws-total-analyst.
           add ws-count-jr-prog              to ws-total-jr-prog.
           add ws-count-sen-prog             to ws-total-sen-prog.
           add ws-count-prog                 to ws-total-prog.
           add ws-count-unclassified         to ws-total-unclassified.
           move 0                            to ws-count-analyst.
           move 0                            to ws-count-sen-prog.
           move 0                            to ws-count-prog.
           move 0                            to ws-count-jr-prog.
           move 0                            to ws-count-unclassified.
  
      * process input and output results until
      *   current page has been filled
      *   OR EOF has been encountered
 
           perform 100-main-logic
             varying ws-line-count from 1 by 1
             until (ws-line-count > ws-lines-per-page
             OR ws-eof = "y").
      * page footer

           write print-line     from ws-page-footer1
             after advancing 2 lines.
      * moves analyst count to different class variables
           move ws-count-analyst             to ws-anal-count.
           move ws-count-sen-prog            to ws-senior-prog-count.
           move ws-count-prog                to ws-prog-count.
           move ws-count-jr-prog             to ws-jr-prog-count.
           move ws-count-unclassified        to ws-unclassified-count.
           write print-line     from ws-page-footer2.


       100-main-logic.
      * Initilaize variables for detail line
           move spaces to print-line.

           move er-employee-name             to pl-employee-name.
           move er-employee-num              to pl-employee-num.
           move er-current-salary            to pl-current-salary.
           move er-years-service             to pl-years-service.

           perform 110-determine-code.
      * calculates the pay increase & salaries
         compute pl-pay-increase rounded = ws-percent-increase *
             er-current-salary / 100.
           compute pl-new-salary rounded = er-current-salary +
             ws-percent-increase * er-current-salary / 100.
           if pl-job-code = "ANALYST"
               compute ws-avg-analyst = ws-avg-analyst +
                 ws-percent-increase * er-current-salary / 100
           else
               if pl-job-code = "SEN PROG"
                   compute ws-avg-sen-prog = ws-avg-sen-prog +
                     ws-percent-increase * er-current-salary / 100
               else
                   if pl-job-code = "PROG"
                       compute ws-avg-prog = ws-avg-prog +
                         ws-percent-increase * er-current-salary / 100
                   else
                       if pl-job-code = "JR PROG"
                           compute ws-avg-jr-prog = ws-avg-jr-prog +
                             ws-percent-increase * er-current-salary /
                             100
                       end-if.



           write print-line after advancing 1 line.

      
      *        reads the logic
      
           read input-file
               at end
                   move "y" to ws-eof.
  

       110-determine-code.
      * shows % sign for the output + different years means
      * different percentage/payment increases for the programmer
           move '%' to pl-increase-sign.
 
           if er-grad           then

           if er-years-16plus   then
                       move 'ANALYST'
                                             to pl-job-code
                       move cnst-percent-analyst
                                             to pl-increase
                       move cnst-percent-analyst
                                             to ws-percent-increase
                   add 1 to ws-count-analyst

               else
           if er-years-7-15
                       move 'SEN PROG'       to pl-job-code
                       move cnst-percent-senior-analyst
                                             to pl-increase
                       move cnst-percent-senior-analyst
                                             to
                         ws-percent-increase
                       add 1 to ws-count-sen-prog
                   else
                       if er-years-3-6
                       move 'PROG'           to pl-job-code
                       move cnst-percent-programmer
                                             to pl-increase
                       move cnst-percent-programmer
                                             to
                             ws-percent-increase
                           add 1             to ws-count-prog
                       else
                       move spaces           to pl-job-code
                       move cnst-percent-unclass
                                             to pl-increase
                       move cnst-percent-unclass
                                             to
                                    ws-percent-increase
                       move spaces           to pl-increase-sign
                           add 1             to ws-count-unclassified
                       end-if
               else
           if er-years-11plus    then
                       move 'PROG'           to pl-job-code
                       move cnst-percent-programmer
                                             to pl-increase
                       move cnst-percent-programmer
                                             to ws-percent-increase
                   add 1 to ws-count-prog
               else
                   if er-years-5-10 then
                       move 'JR PROG'        to pl-job-code
                       move cnst-percent-jr-prog
                                             to pl-increase
                       move cnst-percent-jr-prog
                                             to ws-percent-increase
                       add 1 to ws-count-jr-prog
                   else
                       move spaces           to pl-job-code
                       move cnst-percent-unclass
                                             to pl-increase
                       move cnst-percent-unclass
                                             to ws-percent-increase
                       move spaces
                                             to pl-increase-sign
                       add 1 to ws-count-unclassified
                   end-if
               end-if.

       end program A4.
