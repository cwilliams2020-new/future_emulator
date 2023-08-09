% plot_Figure3_4a.m
% 
% Runs the Archer and Ganopolski 2005 CGSLM with a Latin Hypercube of input
% parameters, plots the output figure and saves model outout for running
% the climate emulator in R. 

%%%% ARCHER + GANOPOLSKI 2005 %%%%
% Runs a simple multi-state climate model and plots the results %
% Based on version II of the model of Paillard 1998 %
% Forced by insolation and influenced by CO2 %

clear

%% Plot original version or bug-fixed version
% orig = 1; % Uncomment for Natalie's original plot
orig =  0; % Uncomment for the bug-fixed version

% Option to save figures
savefig = 0;

% Option to save all valid LHC sea level projections for emulator
outputall = 0;

% Option to output modal scenario rather than lowest RMSE
modal_plot = 0;

%% Load data
% Insolation data (Laskar 2004 June insolation at 65degN)
insol_data_dat = importdata('ForcingData/Insol_Laskar_jul_65N_0.5-1ma.res');
 % Extract insolation data for last and next 500 kyr (double 0 as one of the CO2 values is at 0.1 kyr to capture anthropogenic max)
insol_data_500k_BPAP = vertcat(insol_data_dat.data(1:501,2),insol_data_dat.data(501,2),insol_data_dat.data(502:end,2));

% Load CO2 trajectory data
CO2_data_natural_dat = importdata('ForcingData/CO2_data_Petitetal_-0.4-0ma.res');
CO2_data_rcp26_dat = importdata('ForcingData/CO2_data_rcp26_0-1ma.res');
CO2_data_rcp45_dat = importdata('ForcingData/CO2_data_rcp45_0-1ma.res');
CO2_data_rcp6_dat = importdata('ForcingData/CO2_data_rcp6_0-1ma.res');
CO2_data_rcp85_dat = importdata('ForcingData/CO2_data_rcp85_0-1ma.res');

% Extract CO2 trajectory data
CO2_500k_BP_natural(:,1) = CO2_data_natural_dat.data(:,2);
CO2_1m_AP_dat(:,1) = CO2_data_rcp26_dat.data(:,2);
CO2_1m_AP_dat(:,2) = CO2_data_rcp45_dat.data(:,2);
CO2_1m_AP_dat(:,3) = CO2_data_rcp6_dat.data(:,2);
CO2_1m_AP_dat(:,4) = CO2_data_rcp85_dat.data(:,2);

% Past d18O data
d18O_data_dat = importdata('ForcingData/d18O_LisieckiRaymo_-0.5-0ma.res');
d18O_data = d18O_data_dat.data(:,2);

% Setup time
time_500k_BP_natural = CO2_data_natural_dat.data(:,1); % Extract time data for last 500 kyr from natural data
time_500k_BP = insol_data_dat.data(1:501,1); % Extract time data for last 500 kyr
time_1m_AP = insol_data_dat.data(501:end,1); % Extract time data for next 500 kyr
time_1m_AP(1,1)=0.1; % Change first year of anthropogenic forcing to after persent-day
time_500k_BPAP = vertcat(insol_data_dat.data(1:501,1),0.1,insol_data_dat.data(502:end,1)); % Extract time data for last 500 kyr and next 500 kyr


%% Set some constants
% Set total number of future CO2 scenarios
num_exp_1m_AP = 5; 

% Interpolate natural past CO2 data to give concentration for every 1 kyr
time_500k_BP_natural(end+1,1) = -1; % Create data point for 1 kyr BP
CO2_500k_BP_natural(end+1,1) = 280; % Assign pre-industrial CO2 concentration to 1 kyr BP
time_500k_BP_natural(end+1,1) = 0; % Create data point for 0 kyr BP
CO2_500k_BP_natural(end+1,1) = 280; % Assign pre-industrial CO2 concentration to 0 kyr BP
 
CO2_500k_BP_dat = interp1(time_500k_BP_natural, CO2_500k_BP_natural, time_500k_BP); % Interpolate natural CO2 data to full 500 kyr BP time series
 

%% Run LHC sampling
% There are a number of options here:
% Firstly, the Latin Hypercube can be re-run to reproduce Natalies results
% (use rerun_LHC = 1), it can be run with a larger ensemble (I did this for
% testing parameter sensitivity, use rerun_LHC = 2), or it can simply load
% the selected parameter values that Natalie sent me with this code (use
% rerun_LHC = 0 or any other value).
% 
% Note: If reproducing Natalie's work, don't choose 2 (ideally keep as 1)

if orig == 1
    rerun_LHC = 1;
elseif orig == 0
    % Choose option:
    rerun_LHC = 1; % 1 or 2;
end

if rerun_LHC == 1
    % Set param range
    param_range_1 = [2,11];
    param_range_2 = [1,95];
    param_range_4 = [25,39];
    param_range_5 = [0.1,1.5];
    param_range_8 = [-1,0.4];

    % To reproduce Natalie's LHC, the random number generator must be run 
    % 13 times, however the model need not run for the first 12 times.
    rng default % Reset the random number generator
    % Then re-run LHC generation 13 times
    for r = 1:13
        
        % Run LHC
        n = 1000; % No of sample sets
        zp = 5; % No of dimensions
        lb = [param_range_1(1,1) param_range_2(1,1) param_range_4(1,1) param_range_5(1,1) param_range_8(1,1)];% Lower bounds for parameters
        ub = [param_range_1(1,2) param_range_2(1,2) param_range_4(1,2) param_range_5(1,2) param_range_8(1,2)];% Lower bounds for parameters
        
        %     rng default % For reproducibility - added by ATK-A
        % s = rng
        LHQ = lhsdesign(n,zp,'criterion','maximin'); % Run LHC
        LHQ_samp = bsxfun(@plus,lb,bsxfun(@times,LHQ,(ub-lb))); % Run LHC
        
        LHQ_samp_all = horzcat(LHQ_samp(:,1), LHQ_samp(:,2), zeros(n, 1), LHQ_samp(:,3), LHQ_samp(:,4), zeros(n, 1), zeros(n, 1), LHQ_samp(:,5));
        
        % Natalie's original code has this line, with i0 set at -0.81
%         LHQ_samp_all(n+1,:) = [8, 47, 47, 27, 1.2, 0.35, -0.81, 0]; 
%       %ATKA - there was some discrepancy over the value of i0 (-0.81 or
%         -0.8). This gives consistent figures with the original report.
        LHQ_samp_all(n+1,:) = [8, 47, 47, 27, 1.2, 0.35, -0.8, 0];

    end
    
elseif rerun_LHC == 2
    % Set param range
    param_range_1 = [1,16];
    param_range_2 = [1,195];
    param_range_3 = [1,95];
    param_range_4 = [1,54];
    param_range_5 = [0,2.4];
    param_range_6 = [0,0.7];
    param_range_7 = [-1.6,0];
    param_range_8 = [-1,1];
    
    
    % Run LHC
    n = 10000; % No of sample sets
    zp = 8; % No of dimensions
    lb = [param_range_1(1,1) param_range_2(1,1) param_range_3(1,1) param_range_4(1,1) param_range_5(1,1) param_range_6(1,1) param_range_7(1,1) param_range_8(1,1)];% Lower bounds for parameters
    ub = [param_range_1(1,2) param_range_2(1,2) param_range_3(1,2) param_range_4(1,2) param_range_5(1,2) param_range_6(1,2) param_range_7(1,2) param_range_8(1,2)];% Lower bounds for parameters

    rng default % For reproducibility - added by ATK-A
    % s = rng
    LHQ = lhsdesign(n,zp,'criterion','maximin'); % Run LHC
    LHQ_samp = bsxfun(@plus,lb,bsxfun(@times,LHQ,(ub-lb))); % Run LHC
    
    LHQ_samp_all = horzcat(LHQ_samp(:,1), LHQ_samp(:,2), zeros(n, 1), LHQ_samp(:,3), LHQ_samp(:,4), zeros(n, 1), zeros(n, 1), LHQ_samp(:,5));
    
    % Natalie's original code has this line, with i0 set at -0.81
%     LHQ_samp_all(n+1,:) = [8, 47, 47, 27, 1.2, 0.35, -0.81, 0];
    LHQ_samp_all(n+1,:) = [8, 47, 47, 27, 1.2, 0.35, -0.8, 0];
    
else % Don't re-run the Latin Hypercube
    % This option is for loading the data that Natalie sent me
    paramdir = 'Paillard-A+Goutput/'; % Natalie's original LHC members
    params = importdata([paramdir,'opt_param_values_AG_-0.5_1myr_AP_LHCsamps.res']);
    LHQ_samp_all = str2double(params);
    n = 89;
end


%% Run the model
% Models ice volume change with transitions between 3 states: 
% interglacial (i), mild glacial (g) and full glacial (G) conditions 

% Normalize insolation to zero mean and unit variance
insol_mean_500k_BPAP = mean(insol_data_500k_BPAP); % Calculate mean of insolation data
insol_mean_zero_500k_BPAP = insol_data_500k_BPAP - insol_mean_500k_BPAP; % Normalize insolation data to zero mean
insol_var_500k_BPAP = sqrt(mean((insol_mean_zero_500k_BPAP-(mean(insol_mean_zero_500k_BPAP))).^2)); % Calculate standard deviation of insolation data
insol_var_zero_500k_BPAP = insol_mean_zero_500k_BPAP / insol_var_500k_BPAP ; % Normalize insolation data to zero variance

% Set up blank array for storing RMSE evaluation
RMSE_800k_BP = nan(n+1,4);

% Loop over LHC samples
LHC_row = 1;
param_opt = 0;

for sens_test = 1:(n+1)
    
    % Perform 'smoothed truncation' of normalized insolation data to be used as forcing in model (F)
    a = LHQ_samp_all(sens_test,5); % 1.2; % Parameter #5 for truncation function 
    param_opt(1,5) = 1.2;
    
    F = 0.5 * (insol_var_zero_500k_BPAP + sqrt((4 * a.^2) + (insol_var_zero_500k_BPAP.^2))); % Truncation function to calculate F


    % Normalize truncated insolation to zero mean and unit variance
    F_mean_500k_BPAP = mean(F); % Calculate mean of truncated insolation data
    F_mean_zero_500k_BPAP = F - F_mean_500k_BPAP; % Normalize truncated insolation data to zero mean
    F_var_500k_BPAP = sqrt(mean((F_mean_zero_500k_BPAP-(mean(F_mean_zero_500k_BPAP))).^2)); % Calculate standard deviation of truncated insolation data
    F_var_zero_500k_BPAP = F_mean_zero_500k_BPAP / F_var_500k_BPAP ; % Normalize truncated insolation data to zero variance


    %% Fit CO2 vs i0 values to polynomial model
    CO2_i0_reference = [200 280 400 560]';
    
    % This is the correction of the bug identified by Johan Liakka where there
    % was an inconsistency in i0 between the natural and RCP scenarios (-0.81
    % and -0.7 respectively). Now, all scenarios are consistent with the tuned
    % i0 for the past (-0.81).
    if orig == 1 % Original code with bug (i0 = -0.7 at 280 ppm)
        CO2_i0_reference(:,2) = [-0.3 -0.7 -1.5 -3]';
        x = CO2_i0_reference(:,1);
        y = CO2_i0_reference(:,2);
        model_polyfit = polyfit(x, y, 2);
    elseif orig ==0 % Updated code (i0 = -0.81 at 280 ppm)
%         CO2_i0_reference(:,2) = [-0.3 -0.81 -1.5 -3]';
        CO2_i0_reference(:,2) = [-0.3 -0.8 -1.5 -3]'; %ATKA - updated
        x = CO2_i0_reference(:,1);
        y = CO2_i0_reference(:,2);
        model_polyfit = polyfit(x, y, 3); % Polynomial fit needs to be 3rd order
    end

    fittedX = linspace(min(x), max(x), 100);
    fittedY = polyval(model_polyfit, fittedX);


    % Set up model parameters

    %F_500k_BPAP = insol_var_zero_500k_BPAP(:,1); % Extract normalized insolation data for last and next 500 kyr to use as forcing
    F_500k_BPAP = F_var_zero_500k_BPAP(:,1); % Extract normalized insolation data for last and next 500 kyr to use as forcing
    time = time_500k_BPAP(:,1); % Extract time data for last and next 500 kyr

    %CO2_conc_500k_BP = 280; % Atmospheric CO2 concentration for last 500 kyr (ppm)
    CO2_500k_BP = repmat(CO2_500k_BP_dat, 1, num_exp_1m_AP); % Create time series of CO2 concentrations for time period
    CO2_500k_BP(isnan(CO2_500k_BP)) = 280;

    CO2_conc_1m_AP = [280]; % Atmospheric CO2 concentration for next 500 kyr (ppm)

    CO2_1m_AP = horzcat(repmat(CO2_conc_1m_AP(1,1), length(time_1m_AP),1), CO2_1m_AP_dat); % Create time series of CO2 concentrations for time period


    R_ref_num = [1 2 3]'; % Counter for climate regime (i, g, G)
    R_ref_text = ['i';'g';'G']; % Name of climate regime (i, g, G)
    R_ref_ice = [0 1 1]'; % Reference ice volume for climate regime (i, g, G)


    TR = [LHQ_samp_all(sens_test,1) LHQ_samp_all(sens_test,2) 47]'; % [8 47 47]'; % Time constant #1, 2, 3 for climate regime (i, g, G) (kyr)
    param_opt(1,1) = 8;
    param_opt(1,2) = 47;
    param_opt(1,3) = 47;
    
    TF = LHQ_samp_all(sens_test,4); % 27; % Time constant #4 (kyr)
    param_opt(1,4) = 27;
    
    % This was in the original code but appeared to be inconsistent:
    i0 = -0.8; % Parameter #7 -0.8
    param_opt(1,7) = -0.8; %-0.8
%     % Corrected it to this:
%     i0 = -0.81; % Parameter #7 -0.8
%     param_opt(1,7) = -0.81; %-0.8
    
    i0_BP = repmat(i0, length(time_500k_BP), num_exp_1m_AP); % Insolation threshold for last 500 kyr (in variance units): transition from i -> g when insolation falls below this
    i0_AP(:,1) = repmat(i0, length(time_1m_AP), 1); % Insolation threshold for next 500 kyr (in variance units): transition from i -> g when insolation falls below this
    i0_AP(:,2:5) = polyval(model_polyfit, CO2_1m_AP(:,2:5)); % Insolation threshold for next 500 kyr affected by CO2 concentrations (in variance units): transition from i -> g when insolation falls below this

    i1 = LHQ_samp_all(sens_test,8); % 0; % Insolation threshold #8 (in variance units): transition from G -> i when insolation rises above this 
    param_opt(1,8) = 0;
    
    vmax = 1; % Ice volume threshold: transition from g -> G when ice volume exceeds this


    % Merge past and future data

    CO2_500k_BPAP = vertcat(CO2_500k_BP, CO2_1m_AP);

    i0_BPAP = vertcat(i0_BP, i0_AP);


    % Set up arrays for model output

    R_num{LHC_row} = nan(length(time),num_exp_1m_AP); % Record climate regime number for each kyr
    vR{LHC_row} = nan(length(time),num_exp_1m_AP); % Record reference ice volume for climate regime for each kyr
    dv{LHC_row} = nan(length(time),num_exp_1m_AP); % Record change in ice volume for each kyr
    v{LHC_row} = nan(length(time),num_exp_1m_AP); % Record total ice volume for each kyr
    v0 = 0.35; % Set ice volume to 0.75 #6 (starting conditions for model)
    param_opt(1,6) = 0.35;
    
    param_sens_list(LHC_row,1) = sens_test;


    %% Run model version II

    for col = 1:num_exp_1m_AP % Loop through future CO2 scenarios
        
        for row = 1:length(time) % Loop over time (500 kyr BP - 500 kyr AP)
%             if sens_test == 713 %ATKA - for testing
% %                 a = 1.2; %sens_test; % Parameter for truncation function
% %                 F = 0.5 * (insol_var_zero_500k_BPAP + sqrt((4 * a.^2) + (insol_var_zero_500k_BPAP.^2))); % Truncation function to calculate F
% %                 F_mean_500k_BPAP = mean(F); % Calculate mean of truncated insolation data
% %                 F_mean_zero_500k_BPAP = F - F_mean_500k_BPAP; % Normalize truncated insolation data to zero mean
% %                 F_var_500k_BPAP = sqrt(mean((F_mean_zero_500k_BPAP-(mean(F_mean_zero_500k_BPAP))).^2)); % Calculate standard deviation of truncated insolation data
% %                 F_var_zero_500k_BPAP = F_mean_zero_500k_BPAP / F_var_500k_BPAP ; % Normalize truncated insolation data to zero variance
% %                 F_500k_BPAP = F_var_zero_500k_BPAP(:,1);
% %                 
%                 if row == 1194
%                     disp('Check timestep for natural')
%                 end
%             end
            if row == 1 % If it's the first kyr, set to g conditions (starting conditions for model v2)

                R_num{LHC_row}(row,col) = R_ref_num(2,1); % g
                vR{LHC_row}(row,col) = R_ref_ice(R_num{LHC_row}(row,col),1); % g

%                 dv{LHC_row}(row,col) = (1 - v0) / 50 - F_500k_BPAP(row,1) / TF;
                dv{LHC_row}(row,col) = (1 - v0) / 47 - F_500k_BPAP(row,1) / TF;

                v{LHC_row}(row,col) = v0 + dv{LHC_row}(row,1);

            elseif row > 1 % If it's a subsequent kyr, take into account the climate regime of the previous kyr

                if R_num{LHC_row}(row-1,col) == 1 % Note: when in i conditions, can only transition to g conditions

                    if F_500k_BPAP(row,1) > i0_BPAP(row,col) % If insolation forcing is above i0, remain in i conditions

                        R_num{LHC_row}(row,col) = R_ref_num(1,1); % i
                        vR{LHC_row}(row,col) = R_ref_ice(R_num{LHC_row}(row,col),1); % i
                        dv{LHC_row}(row,col) = (vR{LHC_row}((row-1),col) - v{LHC_row}((row-1),col)) / TR(R_num{LHC_row}(row,col),1) - F_500k_BPAP(row,1) / TF;

                    elseif F_500k_BPAP(row,1) < i0_BPAP(row,col) % If insolation forcing is below i0, transition to g conditions
                        if col == 2
%                             disp(['Glacial commencing at: ',num2str(time(row))])
                        end
                        
                        R_num{LHC_row}(row,col) = R_ref_num(2,1); % g
                        vR{LHC_row}(row,col) = R_ref_ice(R_num{LHC_row}(row,col),1); % g
                        dv{LHC_row}(row,col) = (vR{LHC_row}((row-1),col) - v{LHC_row}((row-1),col)) / TR(R_num{LHC_row}(row,col),1) - F_500k_BPAP(row,1) / TF;

                    else

                        disp "ERROR_a"

                    end

                elseif R_num{LHC_row}(row-1,col) == 2 % Note: when in g conditions, can only transition to G conditions

                    if orig == 1
                        v_samp = v{LHC_row}(row-1,1);
                    elseif orig == 0
                        v_samp = v{LHC_row}(row-1,col);
                    end
                    
%                     if v{LHC_row}(row-1,1) < vmax % If total ice volume is below vmax, remain in g conditions
                    if v_samp < vmax % If total ice volume is below vmax, remain in g conditions

                        R_num{LHC_row}(row,col) = R_ref_num(2,1); % g
                        vR{LHC_row}(row,col) = R_ref_ice(R_num{LHC_row}(row,col),1); % g
                        dv{LHC_row}(row,col) = (vR{LHC_row}((row-1),col) - v{LHC_row}((row-1),col)) / TR(R_num{LHC_row}(row,col),1) - F_500k_BPAP(row,1) / TF;

%                     elseif v{LHC_row}(row-1,1) > vmax % If total ice volume is above vmax, transition to G conditions
                    elseif v_samp > vmax % If total ice volume is above vmax, transition to G conditions

                        R_num{LHC_row}(row,col) = R_ref_num(3,1); % G
                        vR{LHC_row}(row,col) = R_ref_ice(R_num{LHC_row}(row,col),1); % G
                        dv{LHC_row}(row,col) = (vR{LHC_row}((row-1),col) - v{LHC_row}((row-1),col)) / TR(R_num{LHC_row}(row,col),1) - F_500k_BPAP(row,1) / TF;

                    else

                        disp "ERROR_b"

                    end

                elseif R_num{LHC_row}(row-1,col) == 3 % Note: when in G conditions, can only transition to i conditions

                    if F_500k_BPAP(row,1) < i1 % If insolation forcing is below i1, remain in G conditions

                        R_num{LHC_row}(row,col) = R_ref_num(3,1); % G
                        vR{LHC_row}(row,col) = R_ref_ice(R_num{LHC_row}(row,col),1); % G
                        dv{LHC_row}(row,col) = (vR{LHC_row}((row-1),col) - v{LHC_row}((row-1),col)) / TR(R_num{LHC_row}(row,col),1) - F_500k_BPAP(row,1) / TF;

                    elseif F_500k_BPAP(row,1) > i1 % If insolation forcing is above i1, transition to i conditions
                        if col == 2
%                             disp(['Interglacial commencing at: ',num2str(time(row))])
                        end
                        R_num{LHC_row}(row,col) = R_ref_num(1,1); % i
                        vR{LHC_row}(row,col) = R_ref_ice(R_num{LHC_row}(row,col),1); % i
                        dv{LHC_row}(row,col) = (vR{LHC_row}((row-1),col) - v{LHC_row}((row-1),col)) / TR(R_num{LHC_row}(row,col),1) - F_500k_BPAP(row,1) / TF;

                    else

                        disp "ERROR_c"

                    end

                else

                        disp "ERROR_d"
                end

            v{LHC_row}(row,col) = v{LHC_row}((row-1),col) + dv{LHC_row}(row,col);

            end

        end

    end


%% Convert ice volumes to temperature

    T_pre_ind = 0; % Assumed pre-industrial temp (deg C)
    T_lgm = -4; %-6 % Assumed LGM temp (deg C)


    % d18O data

    d18O_pre_ind = d18O_data(501,1); % d18O value for PI
    d18O_lgm = d18O_data(483,1); % d18O value for LGM

    d18O_diff = d18O_lgm - d18O_pre_ind;  % Calculate d18O difference between PI (min) and LGM (max)
    d18O_diff_scaled = d18O_diff / T_lgm; % Scale d18O difference to temp difference

    for row = 1:length(d18O_data)
        T_d18O_data(row,1) = (d18O_data(row,1) - d18O_pre_ind) / d18O_diff_scaled; % Calculate d18O difference from PI and convert to temp
    end


    % Model data BP

    v_lgm_pos = find(v{LHC_row}(471:501,1) == max(v{LHC_row}(471:501,1))) + 470;

    v_pre_ind{LHC_row} = v{LHC_row}(501,1); % Modelled ice value for PI
    v_lgm{LHC_row} = v{LHC_row}(v_lgm_pos,1); % Modelled ice value for LGM

    v_diff{LHC_row} = v_lgm{LHC_row} - v_pre_ind{LHC_row};  % Calculate ice difference between PI(min) and LGM (max)
    v_diff_scaled{LHC_row} = v_diff{LHC_row} / T_lgm; % Scale ice difference to temp difference

    for col = 1:num_exp_1m_AP % Loop through future CO2 scenarios
        for row = 1:length(v{LHC_row})
            T_v_data{LHC_row}(row,col) = (v{LHC_row}(row,col) - v_pre_ind{LHC_row}) / v_diff_scaled{LHC_row}; % Calculate ice difference from PI and convert to temp
        end
    end

    % Model data AP

    CO2_pre_ind = 280; % CO2 value for PI (ppmv)
    T_2xCO2 = 3; %3 % Climate sensitivity (deg C)
    RF_2xCO2 = 3.7; % Radiative forcing (W m-2)

    for col = 1:num_exp_1m_AP % Loop through future CO2 scenarios
        for row = 502:length(v{LHC_row})
            RF_500k_BPAP{LHC_row}(row,col) =  5.35 * log(CO2_500k_BPAP(row,col) / CO2_pre_ind);% Calculate anthropogenic RF increase
            T_anth{LHC_row}(row,col) = T_2xCO2 * RF_500k_BPAP{LHC_row}(row,col) / RF_2xCO2; % Calculate anthropogenic T increase
        end
    end

    T_v_data{LHC_row} = T_v_data{LHC_row} + T_anth{LHC_row}; % Add anthropogenic T signal to modelled signal


%% 

    RMSE_800k_BP(LHC_row,1) = sens_test;
    RMSE_800k_BP(LHC_row,2) = sqrt(mean((T_v_data{LHC_row}(1:501,1) - T_d18O_data).^2));
    RMSE_800k_BP(LHC_row,3) = v{sens_test}(484,1);
    [M,I] = min(T_v_data{LHC_row}(476:491,1));
    RMSE_800k_BP(LHC_row,4) = time_500k_BPAP(I+475,1); % Year of LGM

    LHC_row = LHC_row + 1;

end

%% Compare to univariate optimisation values
% Univariate values from Archer_Ganopolski_2005_rcp_4_1myrAP (RMSE_800k_BP)
param_opt_pos = n+1;
param_opt_orig(1,1) = 1;
param_opt_orig(1,2) = 0.7925; % Mean RMSE for original optimum parameter set
param_opt_orig(1,3) = -17; % Year of LGM
LHC_row = LHC_row - 1;

LGM_T_d18O_pos = find(T_d18O_data(471:501,1) == min(T_d18O_data(471:501,1))) + 470;
LGM_T_d18O(1,2) = time_500k_BPAP(LGM_T_d18O_pos(1,1),1);
LGM_T_d18O(1,3) = T_d18O_data(LGM_T_d18O_pos(1,1),1);

param_opt_final(1,1) = 0;
param_opt_final(1,2) = param_opt_orig(1,2);
param_opt_final(1,3) = 0;
param_opt_final(1,4) = param_opt_orig(1,3);

RMSE_800k_BP_valid = RMSE_800k_BP;

x=0;

for row = 1:LHC_row
    if (RMSE_800k_BP(row,3) <= 1.25) && (RMSE_800k_BP(row,3) >= 1) && (RMSE_800k_BP(row,4) <= -17) && (RMSE_800k_BP(row,4) >= -19) && (RMSE_800k_BP(row,2) <= param_opt_orig(1,2))
        if (RMSE_800k_BP(row,2) <= param_opt_final(1,2))
            param_opt_final = RMSE_800k_BP(row,:);
        end
        
        if ~exist('rows2keep','var')
            rows2keep = row;
        else
            rows2keep = cat(1,rows2keep,row);
        end
        
    else
%         row
       RMSE_800k_BP_valid(row-x,:) = [];
       x = x+1;
    end
end

LHC_row_valid = size(RMSE_800k_BP_valid,1)

% Overwrite which scenario to plot to show modal glacial state, if required
if modal_plot == 1
%     param_opt_final = [927,0.646920800494482,1.11728846780719,-18]; % If choosing best match to modal glacial state over final 500 kyr
%     param_opt_final = [596,0.778964898891300,1.16098930293860,-17]; % If choosing best match to modal glacial state over all future years
%     param_opt_final = [713,0.607771031700368,1.01099299335212,-19]; % If choosing best match to original simulation
    param_opt_final = [541,0.685043442888971,1.15694504544848,-18]; % If choosing best match between RCP4.5 and RCP8.5
end


%% Plot model results (T)

for col_count = 1:LHC_row_valid % Loop through sensitivity tests
    num_regime_shift_natural_valid{col_count} = find(diff([vR{RMSE_800k_BP_valid(col_count,1)}(1,1)-1; vR{RMSE_800k_BP_valid(col_count,1)}(:,1)])); % Identify all transitions to a new climate regime   
    num_regime_shift_rcp26_valid{col_count} = find(diff([vR{RMSE_800k_BP_valid(col_count,1)}(1,2)-1; vR{RMSE_800k_BP_valid(col_count,1)}(:,2)])); % Identify all transitions to a new climate regime   
    num_regime_shift_rcp45_valid{col_count} = find(diff([vR{RMSE_800k_BP_valid(col_count,1)}(1,3)-1; vR{RMSE_800k_BP_valid(col_count,1)}(:,3)])); % Identify all transitions to a new climate regime   
    num_regime_shift_rcp85_valid{col_count} = find(diff([vR{RMSE_800k_BP_valid(col_count,1)}(1,5)-1; vR{RMSE_800k_BP_valid(col_count,1)}(:,5)])); % Identify all transitions to a new climate regime   
end

for col = 1:num_exp_1m_AP % Loop through future CO2 scenarios
    num_regime_shift_opt_orig{col} = find(diff([vR{param_opt_pos}(1,col)-1; vR{param_opt_pos}(:,col)])); % Identify all transitions to a new climate regime
    num_regime_shift_opt_final{col} = find(diff([vR{param_opt_final(1,1)}(1,col)-1; vR{param_opt_final(1,1)}(:,col)])); % Identify all transitions to a new climate regime
end

for col = 1:num_exp_1m_AP % Loop through future CO2 scenarios
    param_non_opt{col} = num_regime_shift_opt_final{col}(11,1) - num_regime_shift_opt_final{col}(10,1);
end

R_num_i_logical = (R_num{param_opt_final(1,1)} == 1);
R_num_i = double(R_num_i_logical);
R_num_i(R_num_i == 0) = nan;
R_num_i(R_num_i(:,1) == 1, 1) = 8.75;
R_num_i(R_num_i(:,2) == 1, 2) = 8;
R_num_i(R_num_i(:,3) == 1, 3) = 7.25;
%R_num_i(R_num_i(:,4) == 1, 4) = 6.5; % RCP6
R_num_i(R_num_i(:,5) == 1, 5) = 6.5;%5.75;

plot_colours = {'k', 'g', 'r', [1 0.5 0], 'b'}; % Set colours for scenarios ([1 0.5 0] orange)
%plot_colours = {[1 0.5 0], 'k', 'g', 'r', 'b'}; % Set colours for scenarios ([1 0.5 0] orange)
sens_plot_colour_BP = [0.75 0.75 0.75];
sens_plot_colour_AP = {[0.75 0.75 0.75], [0.75 1 0.75], [1 0.75 0.75], [0.75 0.75 1]};

col_num=[1 2 3 5]; % Natural RCP2.6 RCP4.5 RCP8.5

fw = 29; 
fh = 7.25;
lw = 1;
fs = 12; 
fs2 = 10;
fs3 = 18;

h = figure('units', 'centimeters', 'position', [3 1 fw fh]);
set(gcf, 'PaperPositionMode', 'auto')

% Plot of climate regimes (via reference ice volumes) and continuous ice volume(bottom panel of Figure 3)

p1 = subplot(1,1,1); line1=plot((time_500k_BPAP(1:501,1))/1000, T_v_data{param_opt_final(1,1)}(1:501,1), 'Color', 'k', 'LineWidth', lw);
set(get(get(line1, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
hold on
for col_count = 1:LHC_row_valid % Loop through sensitivity tests
    col=1;
    line1a=plot((time_500k_BPAP(1:501,1))/1000, T_v_data{RMSE_800k_BP_valid(col_count,1)}(1:501,col), 'Color', sens_plot_colour_BP, 'LineWidth', lw);
    set(get(get(line1a, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
    hold on
end
Ref3 = plot([-0.5 1], [0 0], ':', 'Color', [0.5 0.5 0.5], 'LineWidth', lw);
set(get(get(Ref3, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
line2=plot((time_500k_BPAP(1:501,1))/1000, T_v_data{param_opt_final(1,1)}(1:501,1), 'Color', 'k', 'LineWidth', lw);
set(get(get(line2, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
line3=plot((time_500k_BPAP(1:501,1))/1000, R_num_i(1:501,1), 'Color', 'k', 'Linewidth', 3);
set(get(get(line3, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
for col_count = 1:LHC_row_valid % Loop through sensitivity tests
    for col_count_sens = 1:4 % Loop through future CO2 scenarios
        col=col_num(col_count_sens);
        line1a=plot((time_500k_BPAP(502:end,1))/1000, T_v_data{RMSE_800k_BP_valid(col_count,1)}(502:end,col), 'Color', sens_plot_colour_AP{col_count_sens}, 'LineWidth', lw);
        set(get(get(line1a, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
        hold on
    end
end
for col_count = 1:4 % Loop through future CO2 scenarios
    col=col_num(col_count);
    plot((time_500k_BPAP(502:end,1))/1000, T_v_data{param_opt_final(1,1)}(502:end,col), 'Color', plot_colours{col}, 'LineWidth', lw); 
end
for col_count = 1:4 % Loop through future CO2 scenarios
    col=col_num(col_count);
    plot((time_500k_BPAP(502:end,1))/1000, R_num_i(502:end,col), 'Color', plot_colours{col}, 'Linewidth', 3); 
end
plot((time_500k_BPAP(1:501,1))/1000, T_d18O_data, 'Color', [1 0.5 0], 'LineWidth', lw)
set(gca, 'xtick', -0.5:0.1:1);
set(gca, 'ytick', -8:2:6);
set(gca, 'FontSize', fs, 'LineWidth', lw); 
%xlabel('Time AP (Myr)', 'Fontsize', fs)
ylabel('Temperature anomaly (^{o}C)', 'Fontsize', fs)
axis([-0.5 1 -7 9.5]);
Ref2 = plot([0 0], [-7 9.5], '-', 'Color', [0.5 0.5 0.5], 'LineWidth', lw); 
leg1 = legend({'Natural', 'RCP2.6', 'RCP4.5', 'RCP8.5'});
set(leg1, 'Box', 'off', 'Location', 'northwest', 'Fontsize', fs2);
text(-0.092,1.05,'(a)','units','normalized','FontWeight','bold','Fontsize',fs3);


% Format figure

set(p1,'units','centimeters');

lt=2.5;
bm=1.25;
wd=26;
ht=5.2;
vgp=1.5;

pos1=get(p1,'Position');
pos1(1)=lt;
pos1(2)=bm;
pos1(3)=wd;
pos1(4)=ht;
set(p1,'Position',pos1);


%% Save figure
if savefig == 1
    if orig == 1 
        fileName = 'Plots_orig/Fig3_4a_orig.pdf';
    elseif orig == 0 && rerun_LHC == 1
        fileName = 'Plots/Fig3_4a_corr.pdf';
    elseif orig == 0 && rerun_LHC == 2
        fileName = 'Plots/Fig3_4a_corr_newLHC.pdf';
    end
    
%     print(h, '-dpng', fileName);
    
rez=600; %resolution (dpi) of final graphic
f=gcf; %f is the handle of the figure you want to export
figpos=getpixelposition(f); %dont need to change anything here
resolution=get(0,'ScreenPixelsPerInch'); %dont need to change anything here
set(f,'paperunits','inches','papersize',figpos(3:4)/resolution,'paperposition',[0 0 figpos(3:4)/resolution]); %dont need to change anything here
% path='Plots/'; %the folder where you want to put the file
% name=['Fig3_4b_',ext,'.png']; %what you want the file to be called
% print(f,fullfile(path,name),'-dpng',['-r',num2str(rez)],'-painters') %save file 
% name=['Fig3_4b_',ext,'.pdf']; %what you want the file to be called
print(f,fileName,'-dpdf',['-r',num2str(rez)],'-painters') %save file 


    
end


%% Save data

% Data to be used in emulator
    
    if orig == 1
        ftag = 'Orig';
    else
        ftag = 'Corr';
    end
    
    % Go through each scenario
    for s = [1,2,3,5]
        if s == 1 % natural
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_zero_emissions_upd_1myr_AP.res');
            fName=['Results/emul_inputs_natural.',ftag,'.res'];
            
        elseif s == 2 % RCP2.6
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_rcp26_upd_1myr_AP.res');
            fName=['Results/emul_inputs_RCP26.',ftag,'.res'];
            
        elseif s == 3 % RCP4.5
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_rcp45_upd_1myr_AP.res');
            fName=['Results/emul_inputs_RCP45.',ftag,'.res'];
            
        elseif s == 5 % RCP8.5
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_rcp85_upd_1myr_AP.res');
            fName=['Results/emul_inputs_RCP85.',ftag,'.res'];
            
        end
        
        % Load CO2 and orbits from Natalie's existing input files
        % These are unchanged ? only the 'ice' field needs updated
        emul_inputs = emul_inputs.data(:,1:4);
        
        % Take the ice volume from the CGSLM and apply correction
        % This is taken from the values in plot_Figure3_4b.m
        ice = T_v_data{param_opt_final(1,1)};
        ice = ice([501,503:1502],s);
        ice0=ice;
%         ice = ice/4 * 130; % Correct the temp at the LGM (-4�C) to the GSL at the LSM (-130m)
%         ice(ice>24) = 24;
        
        maxdiff_AG_LTPI = -4;
        maxdiff_SV_LTPI = -129.886;
        maxdiff_AG_MTPI = 2.66; % SAT increase estimated to accompany SLR from PRISM3D (similar to PRISM4) [Haywood et al 2013]
        maxdiff_PRISM_MTPI = 24; % Max SLR from PRISM4 reconstructions [Dowsett et al 2016]
        
        maxdiff_AG_LTPI_scaled = maxdiff_SV_LTPI / maxdiff_AG_LTPI; % Scaled separately depending on state because ice volume not linear
        maxdiff_AG_MTPI_scaled = maxdiff_PRISM_MTPI / maxdiff_AG_MTPI;
        
        ice(ice0<0) = ice(ice0<0) * maxdiff_AG_LTPI_scaled;
        ice(ice0>0 & ice0<=maxdiff_AG_MTPI) = ice(ice0>0 & ice0<=maxdiff_AG_MTPI) * maxdiff_AG_MTPI_scaled;
        ice(ice0>maxdiff_AG_MTPI) = maxdiff_PRISM_MTPI;
        
        ice = round(ice,2); % Added to have same precision as Natalie's data
        
        % Concatenate original orbit and CO2 data with ice volume data
        data = horzcat(emul_inputs,ice);
        
        % Save the file
        headings_text = 'co2 obliquity esinw ecosw ice';
        fileID=fopen(fName,'w');
        fprintf(fileID,'%s\n',headings_text);
        fclose(fileID);
        dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);%,'precision',4
        
    end
    
    
    % Save the Rnum variables
    data = repmat(time_500k_BPAP,1,5);
    data(:,2:5) = R_num{param_opt_final(1,1)}(:,[1:3,5]);
    
    Rnum_text = 'kyr_AP natural RCP2.6 RCP4.5 RCP8.5';
    
    fName=['Results/Rnum_all_scens.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',Rnum_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    % Other outputs from optimised parameter values
if outputall == 1
    
    data = horzcat(time_500k_BPAP,v{param_opt_final(1,1)});
    ice_volume_text = '% kyr_AP / ice_vol_natural / ice_vol_rcp26 / ice_vol_rcp45 / ice_vol_rcp6 / ice_vol_rcp85';
    fName=['Results/ice_volume_AG_-0.5_1myr_AP.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',ice_volume_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = horzcat(time_500k_BPAP,i0_BPAP);
    i0_text = '% kyr_AP / i0_natural / i0_rcp26 / i0_rcp45 / i0_rcp6 / i0_rcp85';
    fName=['Results/i0_AG_-0.5_1myr_AP.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',i0_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = horzcat(time_500k_BPAP,R_num{param_opt_final(1,1)});
    Rnum_text = '% kyr_AP / Rnum_natural / Rnum_rcp26 / Rnum_rcp45 / Rnum_rcp6 / Rnum_rcp85 (1=i, 2=g, 3=G)';
    fName=['Results/Rnum_AG_-0.5_1myr_AP.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',Rnum_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = horzcat(time_500k_BPAP,T_v_data{param_opt_final(1,1)});
    temp_v_text = '% kyr_AP / temp_v_natural / temp_v_rcp26 / temp_v_rcp45 / temp_v_rcp6 / temp_v_rcp85';
    fName=['Results/temp_v_AG_-0.5_1myr_AP.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',temp_v_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = LHQ_samp_all(param_opt_final(1,1),:);
    data(1,3) = param_opt(1,3);
    data(1,6) = param_opt(1,6);
    data(1,7) = param_opt(1,7);
    param_values_text = '% 1 / 2 / 3 / 4 / 5 / 6 / 7 / 8';
    fName=['Results/opt_param_values_AG_-0.5_1myr_AP.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',param_values_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = RMSE_800k_BP(param_opt_final(1,1),:);
    RMSE_text = '% Optimised_LHC_sample_set / av_temp_RMSE_for_500_kyr_BP_compared_to_d18OT_data / ice_vol_at_LGM_17kyrBP (0 = i, 1 = G) / Year_of_LGM_BP';
    fName=['Results/av_T_RMSE_LGM_AG_-0.5_1myr_AP.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',RMSE_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    % Retained LHC sample sets
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = v{RMSE_800k_BP_valid(n,1)}(:,1);
        m = m + 1;
    end
    
    ice_volume_text = '% kyr_AP / ice_vol_natural_for_90_retained_LHC_sample_sets';
    
    fName=['Results/ice_volume_AG_-0.5_1myr_AP_LHCsamps_nat.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',ice_volume_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = v{RMSE_800k_BP_valid(n,1)}(:,2);
        m = m + 1;
    end
    
    ice_volume_text = '% kyr_AP / ice_vol_RCP2.6_for_90_retained_LHC_sample_sets_RCP';
    
    fName=['Results/ice_volume_AG_-0.5_1myr_AP_LHCsamps_RCP26.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',ice_volume_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = v{RMSE_800k_BP_valid(n,1)}(:,3);
        m = m + 1;
    end
    
    ice_volume_text = '% kyr_AP / ice_vol_RCP4.5_for_90_retained_LHC_sample_sets';
    
    fName=['Results/ice_volume_AG_-0.5_1myr_AP_LHCsamps_RCP45.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',ice_volume_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = v{RMSE_800k_BP_valid(n,1)}(:,5);
        m = m + 1;
    end
    
    ice_volume_text = '% kyr_AP / ice_vol_RCP8.5_for_90_retained_LHC_sample_sets';
    
    fName=['Results/ice_volume_AG_-0.5_1myr_AP_LHCsamps_RCP85.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',ice_volume_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = R_num{RMSE_800k_BP_valid(n,1)}(:,1);
        m = m + 1;
    end
    
    Rnum_text = '% kyr_AP / Rnum_natural_for_90_retained_LHC_sample_sets (1=i, 2=g, 3=G)';
    
    fName=['Results/Rnum_AG_-0.5_1myr_AP_LHCsamps_nat.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',Rnum_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = R_num{RMSE_800k_BP_valid(n,1)}(:,2);
        m = m + 1;
    end
    
    Rnum_text = '% kyr_AP / Rnum_RCP2.6_for_90_retained_LHC_sample_sets (1=i, 2=g, 3=G)';
    
    fName=['Results/Rnum_AG_-0.5_1myr_AP_LHCsamps_RCP26.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',Rnum_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = R_num{RMSE_800k_BP_valid(n,1)}(:,3);
        m = m + 1;
    end
    
    Rnum_text = '% kyr_AP / Rnum_RCP4.5_for_90_retained_LHC_sample_sets (1=i, 2=g, 3=G)';
    
    fName=['Results/Rnum_AG_-0.5_1myr_AP_LHCsamps_RCP45.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',Rnum_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = R_num{RMSE_800k_BP_valid(n,1)}(:,5);
        m = m + 1;
    end
    
    Rnum_text = '% kyr_AP / Rnum_RCP8.5_for_90_retained_LHC_sample_sets (1=i, 2=g, 3=G)';
    
    fName=['Results/Rnum_AG_-0.5_1myr_AP_LHCsamps_RCP85.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',Rnum_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = T_v_data{RMSE_800k_BP_valid(n,1)}(:,1);
        m = m + 1;
    end
    
    temp_v_text = '% kyr_AP / temp_v_natural_for_90_retained_LHC_sample_sets';
    
    fName=['Results/temp_v_AG_-0.5_1myr_AP_LHCsamps_nat.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',temp_v_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = T_v_data{RMSE_800k_BP_valid(n,1)}(:,2);
        m = m + 1;
    end
    
    temp_v_text = '% kyr_AP / temp_v_RCP2.6_for_90_retained_LHC_sample_sets';
    
    fName=['Results/temp_v_AG_-0.5_1myr_AP_LHCsamps_RCP26.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',temp_v_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = T_v_data{RMSE_800k_BP_valid(n,1)}(:,3);
        m = m + 1;
    end
    
    temp_v_text = '% kyr_AP / temp_v_RCP4.5_for_90_retained_LHC_sample_sets';
    
    fName=['Results/temp_v_AG_-0.5_1myr_AP_LHCsamps_RCP45.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',temp_v_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = time_500k_BPAP;
    m = 2;
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(:,m) = T_v_data{RMSE_800k_BP_valid(n,1)}(:,5);
        m = m + 1;
    end
    
    temp_v_text = '% kyr_AP / temp_v_RCP8.5_for_90_retained_LHC_sample_sets';
    
    fName=['Results/temp_v_AG_-0.5_1myr_AP_LHCsamps_RCP85.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',temp_v_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    

    data = nan(LHC_row_valid,8);% ATK-A added this as it threw an error in the loop previously
    for n = 1:LHC_row_valid % Loop through valid sample sets
        data(n,:) = LHQ_samp_all(RMSE_800k_BP_valid(n,1),:);
    end
    data(:,3) = param_opt(1,3);
    data(:,6) = param_opt(1,6);
    data(:,7) = param_opt(1,7);
    
    param_values_text = '% 1 / 2 / 3 / 4 / 5 / 6 / 7 / 8';
    
    fName=['Results/opt_param_values_AG_-0.5_1myr_AP_LHCsamps.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',param_values_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
    
    data = RMSE_800k_BP_valid;
    
    RMSE_text = '% 90_retained_LHC_sample_sets / av_temp_RMSE_for_500_kyr_BP_compared_to_d18OT_data / ice_vol_at_LGM_17kyrBP (0 = i, 1 = G) / Year_of_LGM_BP';
    
    fName=['Results/av_T_RMSE_LGM_AG_-0.5_1myr_AP_LHCsamps.',ftag,'.res'];
    fileID=fopen(fName,'w');
    fprintf(fileID,'%s\n',RMSE_text);
    fclose(fileID);
    dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
    
end
    

% Output all valid LHC sea level projections for emulator
if orig == 0
    
    % Go through each valid LHC member
    for i = 1:LHC_row_valid % Comment out when testing
%     for i = 1:10 % Smaller subset for testing purposes
        
        % Select each valid LHC member
        memberid = RMSE_800k_BP_valid(i,1);
        
        if rerun_LHC == 2 
            ftag = ['newLHC_',num2str(i)];
        else
            ftag = num2str(i);
        end
        
        % Go through each scenario
    for s = [1,2,3,5]
        if s == 1 % natural
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_zero_emissions_upd_1myr_AP.res');
            fName=['Results/emul_inputs_natural.',ftag,'.res'];
            
        elseif s == 2 % RCP2.6
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_rcp26_upd_1myr_AP.res');
            fName=['Results/emul_inputs_RCP26.',ftag,'.res'];
            
        elseif s == 3 % RCP4.5
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_rcp45_upd_1myr_AP.res');
            fName=['Results/emul_inputs_RCP45.',ftag,'.res'];
            
        elseif s == 5 % RCP8.5
            emul_inputs = importdata('Emulator/2015_Bristol_5D_v001/orig/Input/2018-08-01 Final report/Samp_orbits_rcp85_upd_1myr_AP.res');
            fName=['Results/emul_inputs_RCP85.',ftag,'.res'];
            
        end
        
        % Load CO2 and orbits from Natalie's existing input files
        % These are unchanged ? only the 'ice' field needs updated
        emul_inputs = emul_inputs.data(:,1:4);
        
        % Take the ice volume from the CGSLM and apply correction
        ice = T_v_data{memberid};
%         ice = ice([501,503:1502],s);
%         ice = ice/4 * 130; % Correct the temp at the LGM (-4�C) to the GSL at the LSM (-130m)
%         ice(ice>24) = 24;
        
        ice = ice([501,503:1502],s);
        ice0=ice;
%         ice = ice/4 * 130; % Correct the temp at the LGM (-4�C) to the GSL at the LSM (-130m)
%         ice(ice>24) = 24;
        
        maxdiff_AG_LTPI = -4;
        maxdiff_SV_LTPI = -129.886;
        maxdiff_AG_MTPI = 2.66; % SAT increase estimated to accompany SLR from PRISM3D (similar to PRISM4) [Haywood et al 2013]
        maxdiff_PRISM_MTPI = 24; % Max SLR from PRISM4 reconstructions [Dowsett et al 2016]
        
        maxdiff_AG_LTPI_scaled = maxdiff_SV_LTPI / maxdiff_AG_LTPI; % Scaled separately depending on state because ice volume not linear
        maxdiff_AG_MTPI_scaled = maxdiff_PRISM_MTPI / maxdiff_AG_MTPI;
        
        ice(ice0<0) = ice(ice0<0) * maxdiff_AG_LTPI_scaled;
        ice(ice0>0 & ice0<=maxdiff_AG_MTPI) = ice(ice0>0 & ice0<=maxdiff_AG_MTPI) * maxdiff_AG_MTPI_scaled;
        ice(ice0>maxdiff_AG_MTPI) = maxdiff_PRISM_MTPI;
        

        
        % Concatenate original orbit and CO2 data with ice volume data
        data = horzcat(emul_inputs,ice);
        
        % Save the file
        headings_text = 'co2 obliquity esinw ecosw ice';
        fileID=fopen(fName,'w');
        fprintf(fileID,'%s\n',headings_text);
        fclose(fileID);
        dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
        
    end
    
        % Save the Rnum variables
        data = repmat(time_500k_BPAP,1,5);
        data(:,2:5) = R_num{memberid}(:,[1:3,5]);
    
        Rnum_text = 'kyr_AP natural RCP2.6 RCP4.5 RCP8.5';
        
        fName=['Results/Rnum_all_scens.',num2str(i),'.res'];
        fileID=fopen(fName,'w');
        fprintf(fileID,'%s\n',Rnum_text);
        fclose(fileID);
        dlmwrite(fName,data,'-append','newline','pc','delimiter',' ','precision',4);
        
    end
end

