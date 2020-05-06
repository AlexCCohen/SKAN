#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/contrib/contrib.hpp>
//#include "highgui.h"
#include <iostream>
#include <string>
//#include <filesystem>
#include <sys/stat.h>

using namespace cv;
using namespace std;

struct Img {
    char name[];
};

extern "C" void print_int(int x) {
    cout << "int: " << x << endl;
}

extern "C" void print_str(char x[]) {
    cout << "str: " << x << endl;
}

extern "C" void initImg(struct Img *img) {
    cout << "Image inited" << endl;
    /*Mat curr;
    curr = imread("test_fish.png", CV_LOAD_IMAGE_COLOR);
    namedWindow( "Display window", WINDOW_AUTOSIZE );
    imshow( "Display window", curr);
    waitKey(0);*/
}

extern "C" struct Img* emptyInitImg() {
    cout << "EmptyImg inited" << endl;
    return NULL;
}

extern "C" struct Img* load(char imageName[])
{
    //make a folder
    //go inside folder and make temp variable name
    mkdir("./temp_directory", 0777);
    /*if (!std::__fs::filesystem::is_directory("tempDir"))
    {
        std::__fs::filesystem::create_directory("tempDir");
    }*/

    string path = string("temp_directory/") + string(imageName);

    Mat img;
    img = imread(imageName, CV_LOAD_IMAGE_COLOR);
    imwrite(path, img);
    struct Img* output = (struct Img*) malloc(sizeof(struct Img));
    strcpy(output->name, imageName);  // Saves imageName without 'tempDir/'
    return output;
}

extern "C" int save(char location[], struct Img* input)
{
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);
    imwrite(location, img);
    return 1;
}

extern "C" int cleanup(struct Img* input)
{
    free(input);
    return 1;
}

extern "C" struct Img* brighten(struct Img* input, int value) {
    //read in temp image
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);

    //modifications
    img = img + value;

    //output temp image
    imwrite(path, img);
    return input;
}

/*extern "C" int dilation(char location[], struct Img* input)
{
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);

    imwrite(location, img);
    return 1;
}*/

extern "C" struct Img* dilation(struct Img* input, int size, int shape) {
    //read in temp image
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);
    //Mat out = img;

    //modifications
    int seShape;
    if(shape == 1) {
        seShape = MORPH_ELLIPSE;
    }
    else if( shape == 2) {
        seShape = MORPH_CROSS;
    }
    else {
        seShape = MORPH_RECT;
    }
    int seSize = size;

    Mat se = getStructuringElement(seShape, Size(2*seSize+1, 2*seSize+1), Point(seSize, seSize));

    //dilate(img, out, se);
    dilate(img,img,se);

    //output temp image
    imwrite(path, img);
    return input;
}

extern "C" struct Img* sobel(struct Img* input) {
    //read in temp image
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);
    Mat out;

    //modifications
    cvtColor(img, out, CV_BGR2GRAY);
    Mat gx, gy;
    Mat absgx, absgy;

    Sobel(out, gx, CV_16S,1,0,3,1,0, BORDER_DEFAULT);
    Sobel(out, gy, CV_16S,0,1,3,1,0, BORDER_DEFAULT);

    convertScaleAbs(gx, absgx);
    convertScaleAbs(gy, absgy);

    addWeighted(absgx, 0.5, absgy, 0.5, 0, out);

    //output temp image
    imwrite(path, out);
    return input;
}

extern "C" struct Img* threshold(struct Img* input, int val) {
    //read in temp image
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);
    Mat out;

    //modifications
    cvtColor(img, out, CV_BGR2GRAY);

    threshold(out, out, val, 255, 0);

    //output temp image
    imwrite(path, out);
    return input;
}

extern "C" struct Img* gaussian(struct Img* input, int val) {
    //read in temp image
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);
    Mat out;

    //modifications
    GaussianBlur(img, out, Size(val,val),0,0);

    //output temp image
    imwrite(path, out);
    return input;
}

extern "C" struct Img* color(struct Img* input, int val) {
    //read in temp image
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread(path, CV_LOAD_IMAGE_COLOR);
    Mat out;

    //modifications
    applyColorMap(img, out, val);

    //output temp image
    imwrite(path, out);
    return input;
}

/*extern "C" int load(char imgName[])
{
    Mat img;
    img=imread("test_fish.png", CV_LOAD_IMAGE_COLOR);

    // Convert image to vector
    vector<uchar> array;
    if (img.isContinuous()) {
        array.assign(img.data, img.data + img.total());
    }
    else {
        for (int i = 0; i < img.rows; ++i) {
            array.insert(array.end(), img.ptr<uchar>(i), img.ptr<uchar>(i)+img.cols);
        }
    }

    // Convert vector to image
    Mat m = Mat(img.rows, img.cols, CV_8UC1);
    memcpy(m.data, array.data(), array.size()*sizeof(uchar));

    namedWindow( "Display window", WINDOW_AUTOSIZE );
    imshow( "Display window", m );

    waitKey(0);
    cout << img.cols << endl;
    return 0;
}*/