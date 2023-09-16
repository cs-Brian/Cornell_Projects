import math
import weakref
import numpy as np
from PIL import Image

############### ---------- Basic Image Processing ------ ##############


### TODO 1: Read an Image and convert it into a floating point array with values between 0 and 1. You can assume a color image
def imread(filename):
    image = Image.open(filename)
    image = image.convert('RGB')
    image_array = np.array(image)
    image_array = np.where(image_array < 256, image_array / 255, image_array)
    #image_array.convert("RGB")
    return image_array


### TODO 2: Convolve an image (m x n x 3 or m x n) with a filter(l x k). Perform "same" filtering. Apply the filter to each channel if there are more than 1 channels
def convolve(img, filt):
    shape = img.shape
    m = shape[0]
    n = shape[1]
    l, k = filt.shape
    l_mid = l // 2
    k_mid = k // 2

    filt_flipped = np.flipud(filt)
    filt_flipped2 = np.fliplr(filt_flipped)

    image = np.zeros_like(img)

    for row in range(m):
        for col in range(n):
            for filt_row in range(l):
                for filt_col in range(k):
                    a = row + filt_row - l_mid
                    b = col + filt_col - k_mid
                    if (a >= 0) and (a < m) and (b >= 0) and (b < n):
                        if len(img.shape) > 2:
                            image[row, col, :] += (
                                img[a, b, :] * filt_flipped2[filt_row, filt_col]
                            )
                        else:
                            image[row, col] += (
                                img[a, b] * filt_flipped2[filt_row, filt_col]
                            )
    return image


### TODO 3: Create a gaussian filter of size k x k and with standard deviation sigma
def gaussian_filter(k, sigma):
    filtered_img = np.array(np.zeros((k, k)))
    center = k // 2
    for row in range(k):
        for col in range(k):
            filtered_img[row][col] = math.exp(
                (float(row - center) ** 2 + float(col - center) ** 2) / (2 * sigma**2 * -1)
            )
    x = np.sum(filtered_img)

    return filtered_img / x


"""
to_greyscale: returns an rgb image converted into greycale image
"""
def to_greyscale(img):
    m =img.shape[0]
    n = img.shape[1]
    image = np.array(np.zeros([m, n]))
    for row in range(m):
        for col in range(n):
            if len(img.shape) > 2:
                value = (
                    0.2125 * img[row][col][0]
                    + 0.7154 * img[row][col][1]
                    + 0.0721 * img[row][col][2]
                )
                image[row][col] = value
            else:
                value = (
                    0.2125 * img[row][0]
                    + 0.7154 * img[row][1]
                    + 0.0721 * img[row][2]
                )
                image[row][col] = value
    return image


### TODO 4: Compute the image gradient.
### First convert the image to grayscale by using the formula:
### Intensity = Y = 0.2125 R + 0.7154 G + 0.0721 B
### Then convolve with a 5x5 Gaussian with standard deviation 1 to smooth out noise.
### Convolve with [0.5, 0, -0.5] to get the X derivative on each channel
### convolve with [[0.5],[0],[-0.5]] to get the Y derivative on each channel
### Return the gradient magnitude and the gradient orientation (use arctan2)
def gradient(img):
    grey_img = to_greyscale(img)
    std_filt = gaussian_filter(5, 1)
    convolved_img = convolve(grey_img, std_filt)

    x_filter = np.array(([[0.5, 0, -0.5]]))
    y_filter = np.array(([[0.5], [0], [-0.5]]))

    x_derivative = convolve(convolved_img, x_filter)
    y_derivative = convolve(convolved_img, y_filter)

    gradient_magnitude = np.sqrt((x_derivative) ** 2 + (y_derivative) ** 2)
    gradient_orientation = np.arctan2(y_derivative, x_derivative)

    return gradient_magnitude, gradient_orientation


##########----------------Line detection----------------

### TODO 5: Write a function to check the distance of a set of pixels from a line parametrized by theta and c. The equation of the line is:
### x cos(theta) + y sin(theta) + c = 0
### The input x and y are arrays representing the x and y coordinates of each pixel
### Return a boolean array that indicates True for pixels whose distance is less than the threshold
def check_distance_from_line(x, y, theta, c, thresh):
    val = np.abs(x * np.cos(theta) + y * np.sin(theta) + c) < thresh
    return val

### TODO 6: Write a function to draw a set of lines on the image. The `lines` input is a list of (theta, c) pairs. Each line must appear as red on the final image
### where every pixel which is less than thresh units away from the line should be colored red
def draw_lines(img, lines, thresh):
    img = img.copy()
    I, J = np.unravel_index(
        np.arange(img.shape[0] * img.shape[1]), (img.shape[0], img.shape[1])
    )
    for (theta, c) in lines:
        index = np.where(check_distance_from_line(J, I, theta, c, thresh))[0]
        img[I[index], J[index], 0] = 1
        img[I[index], J[index], 1] = 0
        img[I[index], J[index], 2] = 0

    return img

### TODO 7: Do Hough voting. You get as input the gradient magnitude and the gradient orientation, as well as a set of possible theta values and a set of possible c
### values. If there are T entries in thetas and C entries in cs, the output should be a T x C array. Each pixel in the image should vote for (theta, c) if:
### (a) Its gradient magnitude is greater than thresh1
### (b) Its distance from the (theta, c) line is less than thresh2, and
### (c) The difference between theta and the pixel's gradient orientation is less than thresh3
def hough_voting(gradmag, gradori, thetas, cs, thresh1, thresh2, thresh3):
    bmap = gradmag > thresh1
    I, J = np.where(bmap)
    votes = np.zeros((len(thetas),len(cs)))
    for i in range(len(thetas)):
        for j in range(len(cs)):
            theta = thetas[i]
            c = cs[j]
            val = check_distance_from_line(J, I, theta, c, thresh2)
            ari = np.abs(gradori[I, J] - theta)
            votes[i, j] = votes[i, j] + np.sum(val & (ari < thresh3))
    return votes


### TODO 8: Find local maxima in the array of votes. A (theta, c) pair counts as a local maxima if (a) its votes are greater than thresh, and
### (b) its value is the maximum in a nbhd x nbhd beighborhood in the votes array.
### Return a list of (theta, c) pairs
def localmax(votes, thetas, cs, thresh, nbhd):
    pair_list = []
    m = votes.shape[0]
    n = votes.shape[1]

    #votes2 = np.zeros_like(votes)

    for row in range(m):
        for col in range(n):
            votes2 = np.array(np.zeros((m,n)))
            for start_nbhd in range(nbhd):
                for end_nbhd in range(nbhd):
                    a = row + start_nbhd - nbhd//2
                    b = col + end_nbhd - nbhd//2
                    if (a >= 0) and (a < m) and (b >= 0) and (b < n):
                        votes2[row][col] = votes[a][b]

            maximum = votes2.max()


            if (maximum <= votes[row][col]) & (votes[row][col] >= thresh):
                pair = (thetas[row], cs[col])
                pair_list.append(pair)
    return pair_list

# Final product: Identify lines using the Hough transform
def do_hough_lines(filename):

    # Read image in
    img = imread(filename)

    # Compute gradient
    gradmag, gradori = gradient(img)

    # Possible theta and c values
    thetas = np.arange(-np.pi - np.pi / 40, np.pi + np.pi / 40, np.pi / 40)
    imgdiagonal = np.sqrt(img.shape[0] ** 2 + img.shape[1] ** 2)
    cs = np.arange(-imgdiagonal, imgdiagonal, 0.5)

    # Perform Hough voting
    votes = hough_voting(gradmag, gradori, thetas, cs, 0.1, 0.5, np.pi / 40)

    # Identify local maxima to get lines
    lines = localmax(votes, thetas, cs, 20, 11)

    # Visualize: draw lines on image
    result_img = draw_lines(img, lines, 0.5)

    # Return visualization and lines
    return result_img, lines
