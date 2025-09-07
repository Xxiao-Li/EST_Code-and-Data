function dydt = Equations(t, y, N, K, G, H, delta, r, epsilon, mP, mZ, Rmax)
    P = y(1:N); % Phytoplankton biomasses
    Z = y(N+1); % Zooplankton biomass
    
    R = Rmax - sum(P)/epsilon; % Nutrient concentration

    dPdt = zeros(N, 1); % Initialize phytoplankton growth rate vector
    for i = 1:N
        dPdt(i) = (r(i) * R / (K + R) - G * (1 - delta(i)) * Z / (H + sum(P)) - mP) * P(i);
    end

    dZdt = (epsilon * G * sum((1 - delta) .* P) / (H + sum(P)) - mZ) * Z; % Zooplankton growth rate

    dydt = [dPdt; dZdt]; % Combine for ODE solver
end
