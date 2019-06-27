% Advanced Macro Project


%----------------------------------------------------------------
% 1. Defining variables and parameters
%----------------------------------------------------------------

var cs        % consumption savers 
    rs        % interest rate savers
    rb        % interest rate borrowers
    ws        % wage savers
    ns        % labour savers
    q         % housing price
    hs        % housing savers
    cb        % consumption borrowers
    wb        % wage borrowers
    nb        % labour borrowers
    hb        % housing borrowers
    cf        % consumption banks
    a         % tecnhology
    y         % output
    d         % savings
    b         % bonds    
    LAMBDA_B  % Lagrange multiplier collateral constraint borrowers
    LAMBDA_F  % Lagrange multipllier capital requirement constraint banks
    dp        % inflation
    x;        % markup


varexo u  % technology shock
       e; % monetary policy shock

parameters BETA_S j ETA BETA_B m BETA_F GAMMA ALPHA RHO_A STDERR_AE KTILDE RHO PHIP PHIY;

%----------------------------------------------------------------
% 2. Calibration
%----------------------------------------------------------------

m          = 0.9     ;    % loan-to-value ratio on housing  
GAMMA      = 1-0.2 ;    % 1-capital requirement ratio 

BETA_S     = 0.995   ;    % savers discount factor    
BETA_B     = 0.985   ;    % borrowers discount factor       
BETA_F     = 0.964   ;    % discount factor banks  
     
j          = 0.1   ;    % housing preference share        
ETA        = 2     ;    % labour elasticity                
ALPHA      = 0.64  ;    % share of savers in the economy  (Assumption)
RHO_A      = 0.9   ;    % AR parameter technology         (Assignment 3)
KTILDE     = 0.0858;    % coefficient on markup in the NKPC
STDERR_AE  = 0.01  ;    % SD shock
RHO        = 0.8   ;    % coefficient for interest-rate smoothing  
PHIP       = 0.5   ;    % coefficient on inflation in Taylor rule
PHIY       = 0.5   ;    % coefficient on output in Taylor rule

%----------------------------------------------------------------
% 3. Model
%----------------------------------------------------------------

model;

%% Savers
  % Euler equation
  1/exp(cs) = BETA_S*exp(rs)/(exp(dp(+1))*exp(cs(+1)));     

  % Labour supply
  exp(ws) = ((exp(ns))^(ETA-1))*exp(cs);

  % Housing demand
  exp(q)/exp(cs) =  j/exp(hs) + BETA_S*exp(q(+1))/exp(cs(+1));

  % Constraint
  exp(cs) + exp(d) + exp(q)*(exp(hs) - exp(hs(-1))) = exp(rs(-1))*exp(d(-1))/exp(dp) + exp(ws)*exp(ns) + ((exp(x)-1)/exp(x))*exp(y);

%% Borrowers
  % Euler equation
  1/exp(cb) = BETA_B*exp(rb(+1))/(exp(dp(+1))*exp(cb(+1))) + exp(LAMBDA_B);

  % Labour supply
  exp(wb) = ((exp(nb))^(ETA-1))*exp(cb);

  % Housing demand
  % There is a typo in Rubio Carrasco 2016. In the collateral constraint (RHS) the denominator should be rb(t), not rb(t+1)
   j/exp(hb) = exp(q)/exp(cb) - BETA_B*exp(q(+1))/exp(cb(+1)) - exp(LAMBDA_B)*(1/exp(rb))*m*exp(q(+1))*exp(dp(+1));

  % Constraint
  exp(cb) + exp(rb)*exp(b(-1))/exp(dp(+1)) + exp(q)*(exp(hb) - exp(hb(-1))) = exp(b) + exp(wb)*exp(nb);

  % Collateral constraint
  exp(b) = m*exp(q(+1))*exp(hb)*exp(dp(+1))/exp(rb);

%% Banks
  % Savers
  1/exp(cf) = BETA_F*exp(rs)/(exp(cf(+1))*exp(dp(+1))) + LAMBDA_F;

  % Borrowers
  1/exp(cf) = BETA_F*exp(rb(+1))/(exp(cf(+1))*exp(dp(+1))) + GAMMA*LAMBDA_F;

%% Firms
  % Production function
  exp(y) = exp(a)*(exp(ns)^ALPHA)*(exp(nb)^(1-ALPHA));                             

  % Labour demand (savers)
  exp(ws) = (1/exp(x))*ALPHA*exp(y)/exp(ns);                        
    
  % Labour demand (borrowers)
  exp(wb) = (1/exp(x))*(1-ALPHA)*exp(y)/exp(nb);    

%% Technology process
  a = RHO*a(-1) + u;       

%% Market clearing
  exp(y) = exp(cs) + exp(cb) + exp(cf);    
  exp(hs) + exp(hb) = 1;
  exp(d) = GAMMA*exp(b);   % Equilibrium in financial markets (this follows from d = (1-CRR)*b but CRR=1-GAMMA so d = (1-(1-GAMMA))*b gives d = GAMMA*b)

%% NKPC
  dp = BETA_S*dp(+1) - KTILDE*x;

%% Taylor rule
  exp(rs) = ((exp(rs(-1)))^RHO)*(((exp(dp)^(1+PHIP))*((exp(y)/exp(y(-1)))^PHIY)*(1/BETA_S))^(1-RHO))*exp(e);
                
end;

%----------------------------------------------------------------
% 4. Computation
%----------------------------------------------------------------

initval;
cs = 0;
rs = 0.01;     
rb = 0.01;     
ws = 0;
ns = 0;
q  = 0;        
hs = 0;      
cb = 0;
wb = 0;
nb = 0;
hb = 0;       
cf = 0;
a  = 0;
y  = 0;
x = 0;
dp = 0;
d = 0;
b = 0;
LAMBDA_B = 0;
LAMBDA_F = 0;
end;
steady;

shocks;
var u;
stderr 100*STDERR_AE;
end;

stoch_simul(periods=1000,order=1,irf=20) y b rs cs cb dp hs hb q;


