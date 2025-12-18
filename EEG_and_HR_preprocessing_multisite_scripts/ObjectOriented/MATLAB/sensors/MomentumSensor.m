classdef MomentumSensor < handle

    methods (Abstract)
        preprocessData(obj)
    end
    
    methods (Static)

        function cost = objective_function(C,tau)
            % Computes the cost based on the squared differences between a shifted matrix C and a set of tau values.
            %
            % Parameters:
            % C - [Array] A matrix or vector where each element represents a shifted timestamp.
            % tau - [Array] A matrix or vector of adjustment factors (delays or advances in time).
            %
            % Returns:
            % cost - [Numeric] The total squared difference cost, representing the alignment error.
            
            % Create a matrix of tau values where each column is filled with the corresponding tau value
            tau0 = tau(1);  % First element of tau
        
            % Calculate the total cost 
            cost = sum((C-tau+tau0).^2); % tau and tau0 get broadcasted
        
        end

        function [tau_opt, fval] = optimizeAlignment(T_ms,T_ns)
            % Optimizes the alignment between two sets of timestamps (in ms and ns) using the fmincon optimization tool.
            %
            % Parameters:
            % T_ms - [Array] Millisecond timestamps.
            % T_ns - [Array] Nanosecond timestamps, presumably needing alignment to T_ms.
            %
            % Returns:
            % tau_opt - [Array] The optimal tau values that minimize the alignment error.
            % fval - [Numeric] The function value (cost) at the solution, indicating the alignment error.
            

            function [cost, grad] = obj_and_grad(C,tau)
                tau0 = tau(1);
                R = (C - tau + tau0);        % residuals
                cost = R'*R;
                % ?J/??_i = -2*R_i  for i>1 , and for i=1 sum(2*R)
                g = -2*R;
                g(1) = 2*sum(R);
                grad = g;
            end

            % tau_initial = -0.05 + 0.1 * rand(height(T_ms), 1);  % Generates n random numbers between -0.05 and 0.05
            tau_initial = T_ns-T_ms+median(T_ns-T_ms); % For faster convergence
            lower_bounds = [];
            upper_bounds = [];
            
            tms0 = T_ms(1);
            C = T_ns - T_ms + tms0;  % Precompute shift
            %% Method 1
            options = optimoptions('fmincon', 'Algorithm', 'sqp' ,'TolFun',1e-4, ...        % stop when cost changes <
                      'TolX',1e-4, ...          % stop when ?-changes
                      'MaxIterations',20, ...  % cap total iterations % 'MaxFunctionEvaluations',9000,...% 
                      'SpecifyObjectiveGradient',true,...
                        'Display', 'off');
            % options = optimoptions('fmincon', 'Algorithm', 'sqp', 'UseParallel', true);
            % options.Display = 'iter-detailed';% For debugging

            
            % [tau_opt,fval] = fmincon(@(tau) MomentumSensor.objective_function(C, tau), tau_initial, [], [], [], [], lower_bounds, upper_bounds, [], options);
            [tau_opt,fval] = fmincon(@(tau) obj_and_grad(C, tau), tau_initial, [], [], [], [], lower_bounds, upper_bounds, [], options);

            % figure; plot(1e3*(T_ns-(T_ms+tau_opt)),'DisplayName','difference+ tau_{opt}');hold on; plot(1e3*(T_ns-T_ms),'DisplayName','original difference'); plot(1e3*tau_opt,'DisplayName','tau_{opt}');legend('show');ylabel('ms');title('By 1e-6 tolerance');fprintf('sumAbs differences %d sumAbs new diff %d \n',sum(abs(T_ns-T_ms)),sum(abs(T_ns-(T_ms+tau_opt))));

        end
        
    end

end