%%% Code for selection-corrected quantile regression
%%% by Manuel Arellano and Stephane Bonhomme

%%%% The code makes use of rq.m

%%% Inputs:
%%% participation indicator D (binary)
%%% outcome variable Y (note: values of Y for D=0 are arbitrary, they may even be missing)
%%% matrix of covariates X, not including the constant
%%% matrix of excluded covariates B, not including the constant.
%%% Z=(X,B) is constructed below

%%% Specification in this version:
%%% propensity score: probit
%%% copula: Gaussian
%%% instrument function: varphi(Z) = propensity score p(Z)
%%% grid of tau's for tau=.2,...,.8
%%% estimates of beta(tau) for tau=.05,...,.95
%%% grid search for rho with 99 equidistant values

%%% Estimate propensity score
% Z=[X B];
% gamma=glmfit(Z,D,'binomial','link','probit');
% pZ=normcdf(gamma(1)+Z*gamma(2:end));

%% Housekeeping
clc
clear all
close all


%% Load R manipulated data incl. propensity score
cps_adults = readtable('../../cps_adults.csv');

% Covariates
ihs_other_faminc = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'ihs_otherfaminc')};
ihs_nonearninc = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'ihs_nonearninc')};
i_chldb6 = str2num(cell2mat(cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_chldb6')}));
i_female = str2num(cell2mat(cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_female')}));
i_white = str2num(cell2mat(cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_white')}));
potexp = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'potexp')};
potexp2 = potexp.^2;

% Dummies
educ_NoHighSchool = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'education_NoHighSchool')};
educ_HighSchool = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'education_HighSchoolGrad')};
educ_Bachelor = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'education_Bachelor')};
educ_Master = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'education_Master')};


% Interactions
fem_ichldb6 = i_chldb6 .* i_female;

X = [i_female, educ_NoHighSchool, educ_Bachelor, educ_Master, i_white, potexp, potexp2];
B = [ihs_other_faminc, ihs_nonearninc, i_chldb6, fem_ichldb6];

Z = [X B];
% propensity score
pZ = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'pZ')};

% participation indicator
D = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_partic')};

% Re estimate participation
gamma=glmfit(Z,D,'binomial','link','probit');
pZ_est =normcdf(gamma(1)+Z*gamma(2:end));

% Check estimates do not vary too much
assert(~any(abs(pZ-pZ_est) > 1e-06), 'Discrepancy between given pZ and re-estimated pZ')

% Observed Wage
Y = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'ihs_incwage')};

%%% Instrument function
varphi=pZ;

%%% Select participants
Y1=Y(D==1);
pZ1=pZ(D==1);
X1=X(D==1,:);
varphi1=varphi(D==1);
[N1 colx]=size(X1);

%%% Percentile values to estimate rho
vectau=(.20:.20:.80)';
[t n]=size(vectau);

%%% Start loop on rho
vecrhoa=(-.98:.02:.98)';
[s n]=size(vecrhoa);

%%% Objective function to be minimized
warning('off')
object=zeros(s,1);
for j=1:s
    rhoa=vecrhoa(j);
    obj=0;
        for k=1:t
            tau=vectau(k);
            %%% Gaussian copula
            G=copulacdf('Gaussian',[tau*ones(N1,1) pZ1],rhoa)./pZ1;
            %%% Rotated quantile regression
            beta=rq([ones(N1,1) X1],Y1,G);
            obj=obj+(mean(varphi1.*((Y1<=beta(1)+X1*beta(2:colx+1))-G)));
         end        
    object(j)=obj^2;
    j
end

%%% Minimize the objective function
[C I]=min(object);
rho=vecrhoa(I);

%%% Optional: plot objective function
%plot(vecrhoa(10:90),object(10:90))

%%% Estimate selection-corrected quantile parameters beta(tau)
%%% Here: at centiles
beta=zeros(colx+1,19);
for tau=.05:.05:.95
    %%% Gaussian copula
    G=copulacdf('Gaussian',[tau*ones(N1,1) pZ1],rho)./pZ1;
    %%% Rotated quantile regression
    beta(:,round(20*tau))=rq([ones(N1,1) X1],Y1,G);
end

%%% Outputs
display('copula parameter') 
rho
display('quantile coefficients') 
beta
%%% Note: the first column in beta corresponds to the intercept. 
%%% different columns are tau=.05 to tau=.95
varnames = {'(Intercept)','female', 'educ_NoHighSchool', 'educ_Bachelor', ...
    'educ_Master', 'white', 'potexp', 'potexp2'};

% Save output coefficients (betas)
T_beta = array2table(beta','VariableNames',varnames)
writetable(T_beta,'../../corrected_quantreg_beta.txt')

