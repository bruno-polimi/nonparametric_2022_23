# nonparametric_2022_23

## lista dataset
- https://www.kaggle.com/datasets/mishra5001/credit-card
- https://www.kaggle.com/datasets/uciml/student-alcohol-consumption
- https://www.kaggle.com/datasets/shebrahimi/financial-distress

## todo
- [x] Cancellare colonne appartamento
- [x] Cancellare colonne EXTERNAL_SOURCE 1 e 3
- [x] Cancellare righe NaN di auto/phone change/ annuity/ NAME_TYPE_SUITE/ CNT_FAMILY_MEMBERS/ EXTERNAL_SOURCE_2/ 
- [x] Trasformare OCCUPATION_TYPE in un factor in cui NaN è interpreato come stringa
- [x] Droppare i 5 casi in cui possiede la macchina ma non è segnato da quanto tempo
- [ ] In GLMnet R c'è una funzione per controllare se il target è ben rappresentato dopo avere diviso il dataset in due parti per training e testing
- [x] Spostare di 1 i dati del credit_bureau e mettere a 0 i NaN
- [x] OWN_CAR_AGE va trasformato con un factor scegliendo opportunamente i bin e avendo una casella per "non ha la macchina"

## Links to data

### clean data
- https://drive.google.com/file/d/1vzOwq4zP1M6WGLKJTftwl-uVxNBA6mBb/view?usp=sharing

### training set
- https://drive.google.com/file/d/1KCP0l760MZAvtKJKo5pF0yvTNOCIjNcP/view?usp=sharing

### test set
- https://drive.google.com/file/d/1eLhO4rrlRLixCWuz1H5Fm3x2ClTKs3ox/view?usp=sharing
