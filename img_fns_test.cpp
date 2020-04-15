/* TODO:
    - Add load_image function */

#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <iostream>
#include <string>
//#include <experimental/optional>
#include <filesystem>

using namespace cv;
using namespace std;

struct Img {
    char name[];
};

struct Img* load(const char imageName[])
{
    //make a folder
    //go inside folder and make temp variable name
    if (!std::__fs::filesystem::is_directory("tempDir"))
    {
        std::__fs::filesystem::create_directory("tempDir");
    }

    string path = string("tempDir/") + string(imageName);

    Mat img;
    img = imread(imageName, CV_LOAD_IMAGE_GRAYSCALE);
    imwrite(path, img);
    struct Img* output = (struct Img*) malloc(sizeof(struct Img));
    strcpy(output->name, path.c_str());
    return output;

}

int main() {
    struct Img* ptr = load("test_fish.png");
}
/*extern "C" int load(char imgName[])
{
    Mat img;
    img=imread("test_fish.png", CV_LOAD_IMAGE_GRAYSCALE);

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