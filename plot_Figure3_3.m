% plot_Figure3_3.m
%
% Plot the forcing data used for the Archer and Ganopolski 2005 simulations
% of past and future climate.
%
% Script updated January 2021 from Natalie's original script
% Archer_Ganopolski_2005_rcp_4_1myrAP.m.
%

clear

%% Plot original version or bug-fixed version
% orig = 1; % Uncomment for Natalie's original plot
orig = 0; % Uncomment for the bug-fixed version

% Option to save figures
savefig = 0;

%% Load data
% Insolation data (Laskar 2004 June insolation at 65degN)
insol_data_dat = importdata('ForcingData/Insol_Laskar_jul_65N_0.5-1ma.res');
% Extract insolation data for last and next 500 kyr (double 0 as one of the CO2 values is at 0.1 kyr to capture anthropogenic max)
insol_data_500k_BPAP = vertcat(insol_data_dat.data(1:501,2),insol_data_dat.data(501,2),insol_data_dat.data(502:end,2));

% Normalize insolation to zero mean and unit variance
insol_mean_500k_BPAP = mean(insol_data_500k_BPAP); % Calculate mean of insolation data
insol_mean_zero_500k_BPAP = insol_data_500k_BPAP - insol_mean_500k_BPAP; % Normalize insolation data to zero mean
insol_var_500k_BPAP = sqrt(mean((insol_mean_zero_500k_BPAP-(mean(insol_mean_zero_500k_BPAP))).^2)); % Calculate standard deviation of insolation data
insol_var_zero_500k_BPAP = insol_mean_zero_500k_BPAP / insol_var_500k_BPAP ; % Normalize insolation data to zero variance

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

% Set total number of future CO2 scenarios
num_exp_1m_AP = 5;

% Past d18O data
d18O_data_dat = importdata('ForcingData/d18O_LisieckiRaymo_-0.5-0ma.res');
d18O_data = d18O_data_dat.data(:,2);

% Setup time
time_500k_BP_natural = CO2_data_natural_dat.data(:,1); % Extract time data for last 500 kyr from natural data
time_500k_BP = insol_data_dat.data(1:501,1); % Extract time data for last 500 kyr
time_1m_AP = insol_data_dat.data(501:end,1); % Extract time data for next 500 kyr
time_1m_AP(1,1)=0.1; % Change first year of anthropogenic forcing to after persent-day
time_500k_BPAP = vertcat(insol_data_dat.data(1:501,1),0.1,insol_data_dat.data(502:end,1)); % Extract time data for last 500 kyr and next 500 kyr


%% Interpolate natural past CO2 data to give concentration for every 1 kyr
% Create data point for 1 kyr BP and 0kyr BP both at pre-industrial CO2
time_500k_BP_natural(end+1,1) = -1;
CO2_500k_BP_natural(end+1,1) = 280;
time_500k_BP_natural(end+1,1) = 0;
CO2_500k_BP_natural(end+1,1) = 280;

% Interpolate natural CO2 data to full 500 kyr BP time series
CO2_500k_BP_dat = interp1(time_500k_BP_natural, CO2_500k_BP_natural, time_500k_BP);


%% Truncation
% Perform 'smoothed truncation' of normalized insolation data to be used as
% forcing in model (F). This is from Paillard 1998, p. 380. He uses a value
% of a = 1, whereas Natalie has chosen a = 1.2, presumably based on a
% sensitivity analysis.

%for sens_test = 0:0.1:2
a = 1.2; %sens_test; % Parameter for truncation function

% Truncation function to calculate F from Paillard 1998
F = 0.5 * (insol_var_zero_500k_BPAP + sqrt((4 * a.^2) + (insol_var_zero_500k_BPAP.^2)));

% Normalize truncated insolation to zero mean and unit variance
F_mean_500k_BPAP = mean(F); % Calculate mean of truncated insolation data
F_mean_zero_500k_BPAP = F - F_mean_500k_BPAP; % Normalize truncated insolation data to zero mean
F_var_500k_BPAP = sqrt(mean((F_mean_zero_500k_BPAP-(mean(F_mean_zero_500k_BPAP))).^2)); % Calculate standard deviation of truncated insolation data
F_var_zero_500k_BPAP = F_mean_zero_500k_BPAP / F_var_500k_BPAP ; % Normalize truncated insolation data to zero variance


%% Fit CO2 vs i0 values to polynomial model
% Reference CO2 levels from A&G 2005 Figure 2:
CO2_i0_reference = [200 280 400 560]';

% This is the correction of the bug identified by Johan Liakka where there
% was an inconsistency in i0 between the natural and RCP scenarios (-0.81
% and -0.7 respectively). Now, all scenarios are consistent with the tuned
% i0 for the past (-0.81). 
if orig == 1
    % For reference, here is Natalie's original code:
    % Reference i0 at different CO2 levels from A&G 2005 Figure 2/text
    CO2_i0_reference(:,2) = [-0.3 -0.7 -1.5 -3]';
    
elseif orig == 0
    % Corrected reference i0 at different CO2 levels (using best fitting parameters for past)
    CO2_i0_reference(:,2) = [-0.3 -0.81 -1.5 -3]';
end

% Fit curve to the reference values
x = CO2_i0_reference(:,1);
y = CO2_i0_reference(:,2);

if orig == 1
    % Natalie used a 2nd order polynomial fit
    model_polyfit = polyfit(x, y, 2);
    
elseif orig ==0
    % Had to increase the degree of the polynomial fit to get a sensible i0 for the future natural scenario
    model_polyfit = polyfit(x, y, 3);
end

fittedX = linspace(min(x), max(x), 100);
fittedY = polyval(model_polyfit, fittedX);

% This fix will affect the i0-CO2 curve slightly compared to what is shown
% in Archer and Ganopolski 2005, Figure 2. This figure can be recreated
% with the following lines:
figure; plot(x, y, 'bo', fittedX, fittedY, 'r-'); % Plot i0 against CO2 concentration
ylabel('i0');xlabel('Atmospheric pCO_2 (ppmv)');title('A&G 2005 Fig. 2')

%% Set up model parameters
%F_500k_BPAP = insol_var_zero_500k_BPAP(:,1); % Extract normalized insolation data for last and next 500 kyr to use as forcing
F_500k_BPAP = F_var_zero_500k_BPAP(:,1); % Extract normalized insolation data for last and next 500 kyr to use as forcing
time = time_500k_BPAP(:,1); % Extract time data for last and next 500 kyr

% Create time series of CO2 concentrations for past time period
% % %CO2_conc_500k_BP = 280; % Atmospheric CO2 concentration for last 500 kyr (ppm)
CO2_500k_BP = repmat(CO2_500k_BP_dat, 1, num_exp_1m_AP); % Same time series for each of the future scenarios
CO2_500k_BP(isnan(CO2_500k_BP)) = 280; % Fill in any blanks

% Create time series for future, repeating pre-industrial levels for
% natural scenario and taking CO2_1m_AP_dat for anthropogenic scenarios
CO2_conc_1m_AP = 280;
CO2_1m_AP = horzcat(repmat(CO2_conc_1m_AP(1,1), length(time_1m_AP),1), CO2_1m_AP_dat);

% Generate i0, accounting for changing CO2 in each scenario:
% This is where the bug issue appears to have arisen: the natural solution
% seems to have been tuned, resulting in an i0 of -0.81 being used.
% However, the CO2 scenarios use the i0 from A&G 2005, i0 = -0.7. 

% Set natural i0:
if orig == 1
    % ORIGINAL CODE
    i0_BP = repmat(-0.81, length(time_500k_BP), num_exp_1m_AP); % Insolation
    % threshold for last 500 kyr (in variance units): transition from i -> g when insolation falls below this % Comment out when testing the issue with i0 (Fix 2)
    i0_AP(:,1) = repmat(-0.81, length(time_1m_AP), 1); % Insolation threshold for next 500 kyr (in variance units): transition from i -> g when insolation falls below this % Comment out when testing the issue with i0 (Fix 2)
    
elseif orig == 0
    % CORRECTION: i0 remains at -0.81 for the natural BP, and fitted value at 280ppm AP (which is also -0.81)
    i0_BP = repmat(-0.81, length(time_500k_BP), num_exp_1m_AP); % Insolation
    % threshold for last 500 kyr (in variance units): transition from i -> g when insolation falls below this % Comment out when testing the issue with i0 (Fix 2)
    i0_AP(:,1) = repmat(fittedY(23), length(time_1m_AP), 1); % Insolation threshold for next 500 kyr (in variance units): transition from i -> g when insolation falls below this % Comment out when testing the issue with i0 (Fix 2)
end

% Set future CO2 scenarios' i0:
i0_AP(:,2:5) = polyval(model_polyfit, CO2_1m_AP(:,2:5)); % Insolation threshold for next 500 kyr affected by CO2 concentrations (in variance units): transition from i -> g when insolation falls below this

% Set i1 and vmax for all scenarios:
i1 = 0; % Insolation threshold (in variance units): transition from G -> i when insolation rises above this
vmax = 1; % Ice volume threshold: transition from g -> G when ice volume exceeds this


% Merge past and future data
CO2_500k_BPAP = vertcat(CO2_500k_BP, CO2_1m_AP);
i0_BPAP = vertcat(i0_BP, i0_AP);


%% Plot forcing data (CO2 + insol)
% Set colours for scenario bars ([1 0.5 0] orange)
plot_colours = {'k', 'g', 'r', [1 0.5 0], 'b'};

% Select which scenarios to plot
col_num=[1 2 3 5]; % Natural RCP2.6 RCP4.5 RCP8.5
% Figure size properties
fw = 29;
fh = 14.5;
lw = 1;
fs = 12;
fs2 = 10;
fs3 = 18;

% Open new figure windo
h = figure('units', 'centimeters', 'position', [3 1 fw fh]);
set(gcf, 'PaperPositionMode', 'auto')

% Subplot of atmospheric CO2 concentration (top panel of Figure 3-3)
p1 = subplot(2,1,1); line1 = plot((time_500k_BPAP(1:501,1))/1000, CO2_500k_BPAP(1:501,1), 'Color', 'k', 'LineWidth', lw);
set(get(get(line1, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
hold on
Ref3 = plot([-0.5 1], [280 280], ':', 'Color', [0.5 0.5 0.5], 'LineWidth', lw);
set(get(get(Ref3, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
line1 = plot((time_500k_BPAP(1:501,1))/1000, CO2_500k_BPAP(1:501,1), 'Color', 'k', 'LineWidth', lw);
set(get(get(line1, 'Annotation'), 'LegendInformation'), 'IconDisplayStyle', 'off')
for col_count = 1:4 % Loop through future CO2 scenarios
    col=col_num(col_count);
    plot((time_500k_BPAP(502:end,1))/1000, CO2_500k_BPAP(502:end,col), '-', 'Color', plot_colours{col}, 'LineWidth', lw);
end
set(gca, 'FontSize', fs, 'LineWidth', lw);
ylabel('Atmospheric pCO_{2} (ppmv)', 'Fontsize', fs)
axis([-0.5 1 0 1200]);
set(gca, 'xtick', -0.5:0.1:1);
set(gca, 'ytick', 0:200:1200);
Ref1 = plot([0 0], [0 1200], '-', 'Color', [0.5 0.5 0.5], 'LineWidth', lw);
leg1 = legend({'Natural', 'RCP2.6', 'RCP4.5', 'RCP8.5'});
set(leg1, 'Box', 'off', 'Location', 'northwest', 'Fontsize', fs2);
text(-0.092,1.05,'(a)','units','normalized','FontWeight','bold','Fontsize',fs3);

% Subplot of normalized and normalized insolation forcing (bottom panel of Figure 3-3)
p2 = subplot(2,1,2); plot((time_500k_BPAP)/1000, F_500k_BPAP, 'LineWidth', lw);
%plot(time_500k_BPAP, insol_var_zero_500k_BPAP, '--');
set(gca, 'FontSize', fs, 'LineWidth', lw);
ylabel('Normalized insolation', 'Fontsize', fs)
xlabel('Time AP (Myr)', 'Fontsize', fs)
axis([-0.5 1 -3 3]);
set(gca, 'xtick', -0.5:0.1:1);
set(gca, 'ytick', -3:1:3);
hold on;
plot((time_500k_BPAP(1:501,1))/1000, i0_BPAP(1:501,1), 'Color', 'k', 'LineWidth', lw);
for col_count = 1:4 % Loop through future CO2 scenarios
    col=col_num(col_count);
    plot((time_500k_BPAP(502:end,1))/1000, i0_BPAP(502:end,col), 'Color', plot_colours{col}, 'LineWidth', lw);
end
Ref1 = plot([0 0], [-3 3], '-', 'Color', [0.5 0.5 0.5], 'LineWidth', lw);
text(0.3, -1.5, '{\iti_{0}}', 'Fontsize', fs)%, 'units', 'normalized');
text(-0.092,1.05,'(b)','units','normalized','FontWeight','bold','Fontsize',fs3);
hold off;


% Format figure

set(p1,'units','centimeters');
set(p2,'units','centimeters');

lt=2.5;
bm=2;
wd=26;
ht=5.2;
vgp=1.5;

pos1=get(p1,'Position');
pos1(1)=lt;
pos1(2)=bm+(ht*1)+(vgp*1);
pos1(3)=wd;
pos1(4)=ht;
set(p1,'Position',pos1);

pos2=get(p2,'Position');
pos2(1)=lt;
pos2(2)=bm;
pos2(3)=wd;
pos2(4)=ht;
set(p2,'Position',pos2);


if savefig == 1
    % Save figure
    rez=600; %resolution (dpi) of final graphic
    f=gcf; %f is the handle of the figure you want to export
    figpos=getpixelposition(f); %dont need to change anything here
    resolution=get(0,'ScreenPixelsPerInch'); %dont need to change anything here
    set(f,'paperunits','inches','papersize',figpos(3:4)/resolution,'paperposition',[0 0 figpos(3:4)/resolution]); %dont need to change anything here
    
    if orig == 1
        path='Plots_orig/'; %the folder where you want to put the file
    elseif orig == 0
        path='Plots/'; %the folder where you want to put the file
    end
    
    name='Fig3-3.png'; %what you want the file to be called
    print(f,fullfile(path,name),'-dpng',['-r',num2str(rez)],'-painters') %save file
    name='Fig3-3.pdf'; %what you want the file to be called
    print(f,fullfile(path,name),'-dpdf',['-r',num2str(rez)],'-painters') %save file
end



