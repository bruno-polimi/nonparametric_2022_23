# nonparametric_2022_23

## lista dataset
- https://www.kaggle.com/datasets/mishra5001/credit-card
- https://www.kaggle.com/datasets/uciml/student-alcohol-consumption
- https://www.kaggle.com/datasets/shebrahimi/financial-distress

## todo
- [ ] Cancellare colonne appartamento
- [ ] Cancellare colonne EXTERNAL_SOURCE 1 e 3
- [ ] Cancellare righe NaN di auto/phone change/ annuity/ NAME_TYPE_SUITE/ CNT_FAMILY_MEMBERS/ EXTERNAL_SOURCE_2/ 
- [ ] Trasformare OCCUPATION_TYPE in un factor in cui NaN è interpreato come stringa
- [ ] Droppare i 5 casi in cui possiede la macchina ma non è segnato da quanto tempo
- [ ] In GLMnet R c'è una funzione per controllare se il target è ben rappresentato dopo avere diviso il dataset in due parti per training e testing
- [ ] Spostare di 1 i dati del credit_bureau e mettere a 0 i NaN
- [ ] OWN_CAR_AGE va trasformato con un factor scegliendo opportunamente i bin e avendo una casella per "non ha la macchina"
