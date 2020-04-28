#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <iostream>
#include <string>
//include <filesystem>
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

extern "C" struct Img* copy(struct Img* input) {
    string path = string("temp_directory/") + string(input->name);
    Mat img = imread("temp_directory/test_fish.png", CV_LOAD_IMAGE_COLOR);

    string new_img = string("copy_") + string(input->name);
    string new_path = string("temp_directory/") + new_img;

    imwrite(new_path, img);

    struct Img* output = (struct Img*) malloc(sizeof(struct Img));
    strcpy(output->name, new_img.c_str());  // Saves imageName without 'tempDir/'
    return output;
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