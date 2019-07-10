model sir

// (1) the world: --------------------------------------------------------------
global {
  int S <- 999; // number of susceptible
  int I <- 1;   // number of infected
  int R <- 0;   // number of recovered
  float beta <- 1.5;   // contact rate
  float gamma <- 0.15; // recovery rate
  int environment_size <- 100; // size of world
  geometry shape <- square(environment_size); // grid
  int neighborhood <- 2; // neighborhood
// The initializer:
	init {
		create individual number: S {
      is_susceptible <- true;
      is_infected <- false;
      is_recovered <- false;
    }
    create individual number: I {
      is_susceptible <-  false;
      is_infected <-  true;
      is_recovered <-  false;
    }
    create individual number: R {
      is_susceptible <-  false;
      is_infected <-  false;
      is_recovered <-  true;
    }
	}
}

// (2) the species: (here only one) --------------------------------------------
species individual  {
	bool is_susceptible;
	bool is_infected;
  bool is_recovered;
  list<individual> neighbours update: self neighbors_at neighborhood;
// each individual has 3 reflex:
  reflex move {
    location <- any_location_in(world.shape);
  }

  reflex become_infected when: is_susceptible {
    float rate  <- 0.0;
		int nb_hosts  <- 0;
		int nb_infected  <- 0;
		loop hst over: neighbours {
			nb_hosts <- nb_hosts + 1;
			if (hst.is_infected) { nb_infected <- nb_infected + 1; }
		}
		if (nb_hosts > 0) { rate <- nb_infected / nb_hosts; }
    if (flip(beta * rate)) {
      is_susceptible <-  false;
      is_infected <-  true;
      is_recovered <-  false;
    }
  }

  reflex become_immune when: (is_infected and flip(gamma)) {
    is_susceptible <- false;
    is_infected <- false;
    is_recovered <- true;
  }
}

// (3) the experiments: --------------------------------------------------------
experiment sir type: gui {
 	parameter "S0" var: S;
  parameter "I0" var: I;
  parameter "R0" var: R;
	parameter "beta" var: beta;
	parameter "gamma" var: gamma;
	output{monitor "S" value: length(individual where(each.is_susceptible));
	       monitor "I" value: length(individual where(each.is_infected));
	       monitor "R" value: length(individual where(each.is_recovered));}
}
