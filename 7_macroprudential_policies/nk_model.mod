% Advanced Macro Project


%----------------------------------------------------------------
% 1. Defining variables and parameters
%----------------------------------------------------------------

var cs        % consumption savers 
    rs        % interest rate savers
    ws        % wage savers
    ns        % labour savers
    q         % housing price
    hs        % housing savers
    cb        % consumption borrowers
    rb        % interest rate borrowers
    wb        % wage borrowers
    nb        % labour borrowers
    hb        % housing borrowers
    cf        % consumption banks
    a         % tecnhology
    y         % output
    d         % savings
    b         % bonds    
    LAMBDA_B  % Lagrange multiplier collateral constraint borrowers
    LAMBDA_F; % Lagrange multipllier capital requirement constraint banks


varexo u; % technology shock

parameters BETA_S j ETA BETA_B m BETA_F GAMMA ALPHA RHO STDERR_AE;

%----------------------------------------------------------------
% 2. Calibration
%----------------------------------------------------------------

BETA_S = 0.995  ;    % savers discount factor          (Iacoviello 2015)
j      = 0.1   ;    % housing preference share        (Iacoviello 2015)
ETA    = 2     ;    % labour elasticity               (Assignment 3)
BETA_B = 0.985  ;    % borrowers discount factor       (Iacoviello 2015)
m      = 0.9   ;    % loan-to-value ratio on housing  (Iacoviello 2015)
BETA_F = 0.964 ;    % discount factor banks           (Iacoviello 2015)
GAMMA  = 1-0.2   ;    % capital requirement ratio       (Iacoviello 2015)
ALPHA  = 0.64  ;   % share of savers in the economy  (Assumption)
RHO    = 0.9   ;    % AR parameter technology         (Assignment 3)
STDERR_AE = 0.01; % SD tecnhology shock

%----------------------------------------------------------------
% 3. Model
%----------------------------------------------------------------

model;

%% Savers
  % Euler equation
  1/exp(cs) = BETA_S*exp(rs)/exp(cs(+1));     

  % Labour supply
  exp(ws) = ((exp(ns))^(ETA-1))*exp(cs);

  % Housing demand
  exp(q)/exp(cs) =  j/exp(hs) + BETA_S*exp(q(+1))/exp(cs(+1));

  % Constraint
  exp(cs) + exp(d) + exp(q)*(exp(hs) - exp(hs(-1))) = exp(rs(-1))*exp(d(-1)) + exp(ws)*exp(ns);

%% Borrowers
  % Euler equation
  1/exp(cb) = BETA_B*exp(rb)/exp(cb(+1)) + exp(LAMBDA_B);

  % Labour supply
  exp(wb) = ((exp(nb))^(ETA-1))*exp(cb);

  % Housing demand
  exp(q)/exp(cb) = j/exp(hb) + BETA_B*exp(q(+1))/exp(cb(+1)) - exp(LAMBDA_B)*m*exp(q(+1))/exp(rb);
  
  % Constraint
  exp(cb) + exp(rb(-1))*exp(b(-1)) + exp(q)*(exp(hb) - exp(hb(-1))) = exp(b) + exp(wb)*exp(nb);

  % Collateral constraint
  exp(b) = m*exp(q(+1))*exp(hb)/exp(rb);

%% Banks
  % Savers
  (1-exp(LAMBDA_F))/exp(cf(+1)) = BETA_F*exp(rs)/exp(cf);

  % Borrowers
  (1-GAMMA*exp(LAMBDA_F))/exp(cf(+1)) = BETA_F*exp(rb)/exp(cf);

%% Firms
  % Production function
  exp(y) = exp(a)*(exp(ns)^ALPHA)*(exp(nb)^(1-ALPHA));                             

  % Labour demand (savers)
  exp(ws) = ALPHA*exp(y)/exp(ns);                        
    
  % Labour demand (borrowers)
  exp(wb) = (1-ALPHA)*exp(y)/exp(nb);    

%% Technology process
  a = RHO*a(-1) + u;       

%% Market clearing
  exp(y) = exp(cs) + exp(cb) + exp(cf);    
  exp(hs) + exp(hb) = 1;
  exp(b) = exp(d);
                    
end;

%----------------------------------------------------------------
% 4. Computation
%----------------------------------------------------------------

initval;
cs = 0;
rs = 0.01;     
ws = 0;
ns = 0;
q  = 0;        
hs = 0;       
cb = 0;
rb = 0.01;     
wb = 0;
nb = 0;
hb = 0;        
cf = 0;
a  = 0;
y  = 0;
end;
steady;

shocks;
var u;
stderr 100*STDERR_AE;
end;

stoch_simul(periods=1000,order=1,irf=20) y b d cs cb cf hs hb q;