
%% The compile state record.
-record(compile, { filename=""
                 , dir=""
                 , base=""
                 , ifile=""
                 , ofile=""
                 , module=[]
                 , code=[]
                 , core_code=[]
                 , abstract_code=[]     %Abstract code for debugger.
                 , options=[]           %Options for compilation
                 , mod_options=[]       %Options for module_info
                 , encoding=none
                 , errors=[]
                 , warnings=[]
                 }).
