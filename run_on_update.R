M_ = stan_model(file="TSIR_model.stan",model_name="tsir_stan")
save(M_,file="compiled_tsir.Rdata")