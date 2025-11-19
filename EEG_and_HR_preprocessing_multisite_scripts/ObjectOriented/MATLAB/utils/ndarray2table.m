function T = ndarray2table(arr, dimnames, dimvalues, ascategorical)
            % Version 1.0.0.0 by Matthias Treder
            % Convert an N-dimensional array into a table. The first dimension defines
            % the number of rows of the table, the other dimensions are pasted underneath. 
            % To code the dimensions, an additional column is created for each
            % dimension. The values in the array are stored in the column "Value".
            %
            % This can be useful if the goal is to plot or further process the data in
            % R or Python using Pandas. R/Pandas mostly work with dataframes i.e.
            % tabular data. A Matlab table can be exported as a .csv file using
            % writetable() and then imported as a dataframe in R or Python.
            % The conversion can also be useful for some Matlab functions such as
            % glmfit.
            % 
            % Parameters:
            % arr       - [s1 x s2 x ... x sN] multidimensional array of N dimensions
            % dimnames  - cell array containing the label for of the N dimensions
            % dimvalues - normally each dimension is numbered as 1 : size of dimension.
            %             If the values have different meanings a cell array can be
            %             provided 
            % ascategorical - if 1, the dimvalues corresponding to every dimension are 
            %               converted into categorical variables (default 1)
            % 
            % Example:
            % % Suppose we measured 10 samples of data. From each sample, we measured
            % % 3 different properties, namely height, weight, and volume. Furthermore,
            % % we measured these properties at 8 different time points and 2
            % % different locations. 
            % % Our 4d-array thus has the dimensions [samples x properties x time x
            % % x location]
            % 
            % X = randn(10, 3, 8, 2);
            % 
            % % Turn into a table providing the names for each dimension
            % dimnames = {'Sample' 'Property' 'Time' 'Location'};
            % T = ndarray2table( X, dimnames);
            % 
            % % To make the table more readable, we like to name the Property and
            % % Location. To this end, we create a NESTED cell array "values" that will
            % % provide the values for each dimension. The number of values must
            % % correspond to the size of the dimension.
            % % We are happy with the numerical coding of Sample and Time, so we leave
            % % the 1st and 3rd element of the values array empty.
            % dimvalues = cell(numel(dimnames), 1);
            % dimvalues{2} = {'height' 'weight' 'volume'};
            % dimvalues{4} = {'Lab1' 'Lab2'};
            % 
            % % Create table and pass values as additional parameter
            % T = ndarray2table( X, dimnames, dimvalues);
            % 
            % % Show first 10 elements of table
            % disp(T(1:10,:))
            %
            % % Create table without turning the dimvalues into categorical variables
            % T2 = ndarray2table( X, dimnames, dimvalues, 0);
            % 
            % % T (which uses categorical data) is much more memory efficient than T2
            % whos T T2
            %
            % % Now you can look at T in the variable viewer to make sure the format is
            % % as desired. Finally, we save the table as CSV file which can be easily 
            % % read in in R or Python.
            % writetable(T, 'mytable.csv')
            ndim = ndims(arr);
            sz = size(arr);
            %% Check input arguments
            if nargin<2 || isempty(dimnames)
                dimnames = strcat('dim', arrayfun( @(ii) {num2str(ii)}, 1:ndim) );
            else
                if numel(dimnames) ~= ndim
                    error('The size of dimnames must correspond to the number of dimensions')
                end
            end
            if nargin<3 || isempty(dimvalues)
                dimvalues = cell(ndim,1);
            else
                if numel(dimvalues) ~= ndim
                    error('Number of dimensions is %d but only %d sets of dimvalues are provided ', ndim, numel(dimvalues))
                end
                for nn=1:ndim
                    if ~isempty(dimvalues{nn}) && numel(dimvalues{nn}) ~= sz(nn)
                        error('Mismatch in ''%s'': Number of dimvalues is %d but the size of this dimension is %d', dimnames{nn}, numel(dimvalues{nn}), sz(nn))
                    end
                end
            end
            if nargin<4 || isempty(ascategorical)
                ascategorical = 1;
            end
            %% Create default dimvalues (if necessary) and convert to categorical (if requested)
            for nn=1:ndim
                if isempty(dimvalues{nn})
                    dimvalues{nn} = 1:sz(nn);
                end
                if ascategorical
                    dimvalues{nn} = categorical(dimvalues{nn});
                end
            end
            %% Create table with first column
            T = table;
            T.Value = arr(:);
            %% Add other columns
            for nn=1:ndim
                if isempty(dimvalues{nn})
                    elems = 1:sz(nn);
                else
                    elems = dimvalues{nn};
                end
                % Create sequence of values for each dimension. The higher the
                % dimension, the slower the numbers must vary
                seq = repelem(elems, prod(sz(1:(nn-1))) );
                % Repeat the sequence
                T.(dimnames{nn}) = repmat(seq', numel(arr)/numel(seq), 1);
            end
        end