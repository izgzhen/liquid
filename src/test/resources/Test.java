import java.io.File;
import java.io.FileReader;
import java.io.IOException;

class Test {
    void main(File f) throws IOException {
        FileReader fileReader = new FileReader(f);
        fileReader.close();
        f.delete();
    }

    void main2(File f1, File f2) throws IOException {
        FileReader fileReader = new FileReader(f1);
        fileReader.close();
        f2.delete();
    }

    void main3() {
        int x = 1;
        System.out.println(x);
    }
}