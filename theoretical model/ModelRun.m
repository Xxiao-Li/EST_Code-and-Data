function Matrix = ModelRun(r, delta, R_sim, nS)
%parameter values
Rmax = R_sim; % mg C m^-3
K = 50; % mg C m^-3
G = 2; % d^-1
H = 80; % mg C m^-3
epsilon = 0.3;
mP = 0.2; % d^-1
mZ = 0.05; % d^-1 for low grazing or 0.04 for high grazing
N = nS; % Number of phytoplankton species

% Initializing state variables
P_init = rand(N,1) * (1 - 0.1) + 0.1; % Uniformly distributed initial conditions for P
Z_init = rand() * (40 - 1) + 1; % Uniformly distributed initial condition for Z
initial_conditions = [P_init; Z_init];

%------- initial conditions -------------------------------------------------;
x0 = initial_conditions; 
tmax  = 10000;
RelT = 1e-10; 
AbsT = 1e-8; 
stepsize = 0.1; 
options = odeset('RelTol',RelT,'AbsTol',AbsT,'MaxStep',stepsize);
f     = @(t,y) Equations(t, y, N, K, G, H, delta, r, epsilon, mP, mZ, Rmax);
[t,y]  = ode45(f,[0:stepsize:tmax],x0,options);
Matrix = [t,y] ;

end
