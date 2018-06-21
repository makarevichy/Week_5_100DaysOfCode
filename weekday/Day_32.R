library(keras)

path_img <- c('~/dataset/cats/img')
path_train <- c('~/dataset/cats/train')
path_val <- c('~/dataset/cats/valid')
path_test <- c('~/dataset/cats/test')
cats <- c('/cats')
dogs <- c('/dogs')
path_train_cats <- paste0(path_train, cats)
path_train_dogs <- paste0(path_train, dogs)
path_val_cats <- paste0(path_val, cats)
path_val_dogs <- paste0(path_val, dogs)

# if(!dir.exists(path_train)){
# dir.create(path_train)
# }
# if(!dir.exists(path_val)){
#   dir.create(path_val)
# }
# if(!dir.exists(path_test)){
#    dir.create(path_test)
#   }
# dir.create(path_train_cats)
# dir.create(path_train_dogs)
# dir.create(path_val_cats)
# dir.create(path_val_dogs)

test_dir <- file.path(base_dir, "test")
dir.create(test_dir)
path_test_cats <- file.path(path_test, 'cats')
dir.create(path_test_cats)
path_test_dogs <- file.path(path_test, 'dogs')
dir.create(path_test_dogs)

fnames <- paste0("cat.", 1:1000, ".jpg")
file.copy(file.path(path_img, fnames), 
  file.path(path_train_cats))
fnames <- paste0("cat.", 1001:1500, ".jpg")
file.copy(file.path(path_img, fnames), 
  file.path(path_val_cats))
fnames <- paste0("cat.", 1501:2000, ".jpg")
file.copy(file.path(path_img, fnames),
  file.path(path_test_cats))
fnames <- paste0("dog.", 1:1000, ".jpg")
file.copy(file.path(path_img, fnames),
  file.path(path_train_dogs))
fnames <- paste0("dog.", 1001:1500, ".jpg")
file.copy(file.path(path_img, fnames),
  file.path(path_val_dogs)) 
fnames <- paste0("dog.", 1501:2000, ".jpg")
file.copy(file.path(path_img, fnames),
  file.path(path_test_dogs))

conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(100, 100, 3)
)

model <- keras_model_sequential() %>% 
  conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

freeze_weights(conv_base)
length(model$trainable_weights)

train_datagen <-  image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)

test_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  path_train,                  # Target directory  
  train_datagen,              # Data generator
  target_size = c(100, 100),  # Resizes all images to 150 × 150
  batch_size = 20,
  class_mode = "binary"       # binary_crossentropy loss for binary labels
)

validation_generator <- flow_images_from_directory(
  path_val,
  test_datagen,
  target_size = c(100, 100),
  batch_size = 20,
  class_mode = "binary"
)

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 2e-5),
  metrics = c("accuracy")
)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 15,
  validation_data = validation_generator,
  validation_steps = 50, 
  callbacks = callback_early_stopping(patience = 2)
)