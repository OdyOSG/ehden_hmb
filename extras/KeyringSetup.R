# Keyring Setup -------------

configBlock <- "[Place configblock name]" # name of config block

keyringName <- "ehden_hmb"
keyringPassword <- "" # This password is simply to avoid a prompt when creating the keyring

# Create the keyring if it does not exist.
# If it exists, clear it out so we can re-load the keys
allKeyrings <- keyring::keyring_list()
if (keyringName %in% allKeyrings$keyring) {
  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }
  # Delete all keys from the keyring so we can delete it
  cli::cat_bullet("Delete existing keyring: ", keyringName,
                  bullet = "warning", bullet_col = "yellow")
  keys <- keyring::key_list(keyring = keyringName)
  if (nrow(keys) > 0) {
    for (i in 1:nrow(keys)) {
      keyring::key_delete(keys$service[i], keyring = keyringName)
    }
  }
  keyring::keyring_delete(keyring = keyringName)
}
# set a new keyring for study
keyring::keyring_create(keyring = keyringName, password = keyringPassword)


creds <- c(
  "dbms", # the database dialect
  "user", # the user name for the db
  "password", # the password for the db
  "connectionString", # the connection string to access the db
  "cdmDatabase", # the database hosting the cdm
  "cdmSchema", # the schema hosting the cdme
  "vocabSchema", # the schema hosting the vocabulary, usually same as cdm
  "workDatabase", # the database hosting the work or scratch
  "workSchema", # the schema hosting the work or scratch
  "role" # the role id for your user in the db
)

cred_block <- paste(configBlock, creds, sep = "_")

# set a new keyring for study
purrr::walk2(cred_block, creds, ~keyring::key_set(service = .x, keyring = keyringName, prompt = .y))
