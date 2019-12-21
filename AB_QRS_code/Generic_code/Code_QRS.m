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

%%% Load R manipulated data incl. propensity score
cps_adults = readtable('../../cps_adults.csv');

% Covariates
other_faminc = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'other_faminc')};
nonearninc = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'nonearninc')};
i_chldb6 = str2num(cell2mat(cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_chldb6')}));
i_female = str2num(cell2mat(cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_female')}));
i_white = str2num(cell2mat(cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_white')}));
nchld1 = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'nchld1')};
nchld6 = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'nchld6')};
nchld18 = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'nchld18')};
nchldtot = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'nchldtot')};
age = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'AGE')};
potexp = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'potexp')};

X = [i_female, i_white, age, potexp];
B = [other_faminc, nonearninc, nchld1, nchld6, nchld18];

Z = [X B];
% propensity score
pZ = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'pZ')};

% participation indicator
D = str2num(cell2mat(cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'i_partic')}));

% Observed Wage
Y = cps_adults{:,find(string(cps_adults.Properties.VariableNames) == 'INCWAGE')};

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
