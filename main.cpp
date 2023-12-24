#include <cmath>
#include <iostream>

using namespace std;
const double limit = 0.000001;
class Matrix{
protected:
    int rows, columns;
    double **data{};
    bool change = true;
public:
    Matrix() {
        rows = 0;
        columns = 0;
    }
    Matrix(int row, int column) {
        rows = row;
        columns = column;
        data = new double* [rows];
        for (int i = 0; i < rows; i++) {
            data[i] = new double[columns];
        }
        for (int i = 0; i < rows; i++){
            for (int j = 0; j < columns; j++){
                data[i][j] = 0.0;
            }
        }
    }
    void printMatrix(){
        for (int i = 0; i < rows; i++){
            for (int j = 0; j < columns; j++){
                cout << data[i][j] << " ";
            }
            cout << endl;
        }
    }
    void setElement(int row, int column, double elem){
        this->data[row][column] = elem;
    }
    int getRows() const{
        return rows;
    }
    int getColumns() const{
        return columns;
    }
    double getElement(int row, int column){
        return data[row][column];
    }
    Matrix operator+(const Matrix &);
    Matrix operator-(const Matrix &);
    Matrix& operator=(Matrix otherMatrix);
    Matrix operator*(const Matrix &);
    double findMaxColumn(int j){
        float max = 0;
        int index = 0;
        for (int i = j; i < rows; i++){
            if (abs(data[i][j]) > max){
                max= abs(data[i][j]);
                index = i;
            }
        }
        return index;
    }
    static Matrix transpose(const Matrix &);
    friend istream& operator >>(istream&in, Matrix &matrix);
    friend ostream& operator <<(ostream&out, Matrix &matrix);
};
istream& operator>>(istream&in, Matrix &matrix){
    for(int i = 0; i < matrix.rows; i++){
        for (int j = 0; j < matrix.columns; j++){
            in >> matrix.data[i][j];
        }
    }
    return in;
}
ostream& operator<<(ostream&out, Matrix &matrix) {
    for(int i = 0; i < matrix.rows; i++){
        for (int j = 0; j < matrix.columns; j++){
            cout.precision(2);
            out << fixed << matrix.data[i][j] <<" ";
        }
        out << endl;
    }
    return out;
}

Matrix Matrix::operator+(const Matrix &otherMatrix) {
    if (otherMatrix.rows != rows || otherMatrix.columns != columns) {
        this->change = false;
        cout << "Error: the dimensional problem occurred\n";
        return *this;
    } else {
        Matrix temp(this->rows, this->columns);
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                temp.setElement(i, j, this->data[i][j] + otherMatrix.data[i][j]);
            }
        }
        this->change = true;
        return temp;
    }
}
Matrix Matrix::operator-(const Matrix &otherMatrix) {
    if (otherMatrix.rows != rows || otherMatrix.columns != columns) {
        cout << "Error: the dimensional problem occurred\n";
        this->change = false;
        return *this;
    } else {
        Matrix temp(this->rows, this ->columns);
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                temp.setElement(i, j, this->data[i][j] - otherMatrix.data[i][j]);
            }
        }
        this->change = true;
        return temp;
    }

}
Matrix& Matrix::operator=(Matrix otherMatrix) {
    if (this->rows == otherMatrix.rows && this->columns == otherMatrix.columns){
        for (int i = 0; i < this->rows; i++){
            for (int j = 0; j < this->columns; j++){
                if (fabs(otherMatrix.data[i][j]) < limit){
                    this->data[i][j] = 0.0;
                } else {
                    this->data[i][j] = otherMatrix.data[i][j];
                }
            }
        }
        if (otherMatrix.change) {
            cout << *this;
        }
        return *this;

    }
    return *this;
}
Matrix Matrix::operator*(const Matrix &otherMatrix){
    if (this->columns == otherMatrix.rows){
        Matrix temp(this->rows, otherMatrix.columns);
        for (int i = 0; i < this->rows; i++){
            for (int j = 0; j < otherMatrix.columns; j++){
                for (int k = 0; k < this->columns; k++){
                    temp.setElement(i,j, temp.getElement(i,j) + this->data[i][k] * otherMatrix.data[k][j]);
                }
            }
        }
        this->change = true;
        return temp;
    } else{
        cout << "Error: the dimensional problem occurred\n";
        this->change = false;
        return *this;
    }
}
Matrix Matrix::transpose(const Matrix &otherMatrix) {
    Matrix temp(otherMatrix.columns, otherMatrix.rows);
    for(int i = 0; i < otherMatrix.rows; i++){
        for (int j = 0; j < otherMatrix.columns; j++){
            temp.setElement(j, i,  otherMatrix.data[i][j]);
        }
    }
    return temp;
}
class squaredMatrix: public Matrix{
public:
    squaredMatrix(){
        rows = columns = 0;
    }
    explicit squaredMatrix(int n){
        rows = columns = n;
        data = new double* [rows];
        for (int i = 0; i < n; i++){
            data[i] = new double[columns];
        }
        for (int i = 0; i < n; i ++){
            for (int j = 0; j < n; j++){
                data[i][j] = 0;
            }
        }
    }
    squaredMatrix operator+(squaredMatrix square){
        Matrix* first = this;
        Matrix* second  = &square;
        Matrix multiplication = *first + *second;
        squaredMatrix *ans = (squaredMatrix*) &multiplication;
        return *ans;
    }
    squaredMatrix operator*(squaredMatrix square){
        Matrix* first = this;
        Matrix* second  = &square;
        Matrix multiplication = *first * *second;
        squaredMatrix *ans = (squaredMatrix*) &multiplication;
        return *ans;
    }
    void subMatrix(squaredMatrix& m, squaredMatrix& temp, int p, int q, int n){
        int i = 0, j = 0;
        for (int r = 0; r < n; r ++){
            for (int c = 0; c < n; c++){
                if (r != p && c != q){
                    temp.setElement(i, j++, m.getElement(r, c));
                    if (j == n - 1){
                        j = 0;
                        i ++;
                    }
                }
            }
        }
    }
    double determinant (squaredMatrix &square, int n){
        double d = 0;
        if (n == 1){
            return square.getElement(0,0);
        }
        int sign = 1;
        squaredMatrix *temp = new squaredMatrix(square.rows);
        for (int k = 0; k < n; k++){
            subMatrix(square, *temp, 0, k , n);
            d += sign * square.getElement(0,k) * determinant(*temp, n - 1);
            sign = -sign;
        }
        return d;
    }
    squaredMatrix operator-(squaredMatrix square){
        Matrix* first = this;
        Matrix* second  = &square;
        Matrix minus = *first - *second;
        squaredMatrix *ans = (squaredMatrix*) &minus;
        return *ans;

    }
    squaredMatrix operator=(squaredMatrix square){
        Matrix* first = this;
        Matrix* second  = &square;
        *first = *second;
        squaredMatrix *ans = (squaredMatrix*) &first;
        return *ans;
    }
    static squaredMatrix transpose(squaredMatrix square){
        Matrix* matrix = &square;
        Matrix trans = trans.transpose(*matrix);
        squaredMatrix *ans = (squaredMatrix*) &trans;
        return *ans;
    }
};
class IdentityMatrix: public squaredMatrix{
public:
    IdentityMatrix(){
        rows = columns = 0;
    }
    explicit IdentityMatrix(int n){
        rows = columns = n;
        data = new double* [rows];
        for (int i = 0; i < n; i++){
            data[i] = new double [rows];
        }
        for (int i = 0; i < n; i++){
            for (int j = 0; j < n; j++){
                if (i == j){
                    data[i][j] = 1;
                } else {
                    data[i][j] = 0;
                }
            }
        }
    }
};
class EliminationMatrix: public IdentityMatrix{
public:
    EliminationMatrix(squaredMatrix &squaredMatrix, int n, int m){
        IdentityMatrix *identityMatrix = new IdentityMatrix(squaredMatrix.getRows());
        identityMatrix->setElement(n, m, -((float)squaredMatrix.getElement(n, m)/(float)squaredMatrix.getElement(m, m)));
        rows = columns = squaredMatrix.getRows();
        data = new double* [rows];
        for (int i = 0; i < rows; i++){
            data[i] = new double [rows];
        }
        for (int i = 0; i < rows; i++){
            for (int j = 0; j < columns; j++){
                data[i][j] = identityMatrix->getElement(i, j);
            }
        }
    }
};
class PermutationMatrix: public IdentityMatrix{
public:
    PermutationMatrix(squaredMatrix &squaredMatrix, int n, int m){
        IdentityMatrix *identityMatrix = new IdentityMatrix(squaredMatrix.getRows());
        rows = columns = squaredMatrix.getColumns();
        data = new double*[rows];
        for (int i = 0; i < rows; i++){
            data[i] = new double [rows];
        }
        for (int i = 0; i < rows; i++){
            for (int j = 0; j < columns; j++){
                if (i == n){
                    if (j == m){
                        data[i][j] = 1;
                    } else {
                        data[i][j] = 0;
                    }
                } else if (i == m){
                    if (j == n){
                        data[i][j] = 1;
                    } else {
                        data[i][j] = 0;
                    }
                } else if (i == j){
                    data [i][j] = 1;
                } else {
                    data[i][j] = 0;
                }
            }
        }
    }
};
class ColumnVector {
private:
    int rows{};
    double *data{};
public:
    ColumnVector(){
        rows = 0;
    }
    ColumnVector(int n){
        rows = n;
        data = new double[rows];
        for (int i = 0; i < rows; i++){
            data[i] = 0.00;
        }
    }
    friend istream& operator>>(istream&in, ColumnVector &columnVector){
        for(int i = 0; i < columnVector.rows; i++){
            string s;
            in >> s;
            columnVector.data[i] = stod(s);
        }
        return in;
    }
    friend ostream& operator<<(ostream&out, ColumnVector &columnVector) {
        for(int i = 0; i < columnVector.rows; i++){
            cout.precision(2);
            cout << fixed << columnVector.data[i] << endl;
        }
        return cout;
    }
    void printColumnVector(){
        for (int i = 0; i < this->rows; i++){
            cout.precision(2);
            cout << fixed << data[i] << endl;
        }
    }
    ColumnVector operator*(squaredMatrix otherMatrix){
        ColumnVector temp(otherMatrix.getRows());
        for (int i = 0; i < this->rows; i++) {
            double t = 0.00;
            for (int j = 0; j < this->rows; j++) {
                t += otherMatrix.getElement(i,j) * this->data[j];
            }
            temp.data[i] = t;
            if (fabs(temp.data[i]) < limit ){
                temp.data[i] = 0.00;
            }
        }
        return temp;
    }
    void normalization(squaredMatrix &otherMatrix){
        for (int i = 0; i < otherMatrix.getRows(); i++) {
            this->data[i] = this->data[i]/otherMatrix.getElement(i,i);
            if (fabs(this->data[i]) < limit ){
                this->data[i] = 0.00;
            }
            otherMatrix.setElement(i, i, 1.00);
        }
    }
};


/*
 * Different size of matrices
 * Your output has to contain:
D = A+B
E = B-A
F = C*A
G = AT
 */
void simple_operations(){
    int n, m;
    cin >> n; cin >> m;
    Matrix matrixA(n, m);
    cin >> matrixA;
    cin >> n; cin >> m;
    Matrix matrixB(n, m);
    cin >> matrixB;
    cin >> n; cin >> m;
    Matrix matrixC(n, m);
    cin >> matrixC;
    Matrix matrixD(matrixA.getRows(), matrixA.getColumns());
    matrixD = matrixA+matrixB;
    Matrix matrixE(matrixB.getRows(), matrixB.getColumns());
    matrixE = matrixB-matrixA;
    Matrix matrixF(matrixC.getRows(), matrixA.getColumns());
    matrixF = matrixC * matrixA;
    Matrix matrixG(matrixA.getColumns(), matrixA.getRows());
    matrixG = matrixG.transpose(matrixA);
}
/*
 * Squared matrices
 * Your output has to contain:
D = A+B
E = B-A
F = C*A
G = AT
 */
void simple_operations_squared_matrices(){
    int n, m;
    cin >> n;
    squaredMatrix matrixA(n);
    cin >> matrixA;
    cin >> n;
    squaredMatrix matrixB(n);
    cin >> matrixB;
    cin >> n;
    squaredMatrix matrixC(n);
    cin >> matrixC;
    squaredMatrix matrixD(matrixA.getRows());
    matrixD = matrixA+matrixB;
    squaredMatrix matrixE(matrixB.getRows());
    matrixE = matrixB-matrixA;
    squaredMatrix matrixF(matrixC.getRows());
    matrixF = matrixC * matrixA;
    squaredMatrix matrixG(matrixA.getColumns());
    matrixG = matrixG.transpose(matrixA);

}

/*
 * The input contains:
A square matrix A.
 * Your output has to contain:
I^3x3
E_21 for A
B = E_21*A
P_21 for A
C=P_21*A
 */

void elimination_permutation_matrices(){
    int n;
    cin >> n;
    squaredMatrix matrixA(n);
    cin >> matrixA;
    IdentityMatrix *identityMatrix = new IdentityMatrix(3);
    identityMatrix->printMatrix();
    EliminationMatrix *eliminationMatrix = new EliminationMatrix(matrixA, 1, 0);
    eliminationMatrix->printMatrix();
    squaredMatrix matrixB(n);
    matrixB = *eliminationMatrix * matrixA;
    PermutationMatrix *permutationMatrix = new PermutationMatrix(matrixA, 1, 0);
    permutationMatrix->printMatrix();
    squaredMatrix matrixC(n);
    matrixC = *permutationMatrix * matrixA;
}
/*
 * Input format:
A square matrix A.
 * Output format:
After each transformation (row permutation or elimination) print out the modified A-matrix splitting the output segments with a line:

«step #k: elimination» or «step #k: permutation»

where k is the step number.

The final answer should be also placed within the new section:

«result:»
 */
void determinant(){
    int n;
    cin >> n;
    squaredMatrix matrixA(n);
    cin >> matrixA;
    squaredMatrix tempMatrix(n);
    int step = 0;
    for (int k = 0; k < n - 1; k++) {
        step++;
        cout << "step #" << step << ": permutation" << endl;
        int f = matrixA.findMaxColumn(k);
        PermutationMatrix *permutationMatrix = new PermutationMatrix(matrixA, k, f);
        matrixA = *permutationMatrix * matrixA;
        for (int i = 0; i < n - k - 1; i++) {
            step++;
            cout << "step #" << step << ": elimination" << endl;
            EliminationMatrix *eliminationMatrix;
            eliminationMatrix = new EliminationMatrix(matrixA, k + i + 1, k);
            matrixA = *eliminationMatrix * matrixA;
        }
    }
    cout << "result:\n" << matrixA.determinant(matrixA, matrixA.getRows());
}

/*
 * Input format
A square matrix A.
 * Output format
Compose and print the augmented matrix for a given one as «step #0: Augmented Matrix».
Code the direct elimination. Entitle the section with the line «Direct way:». Describe the steps as in the previous exercise (see the examples).
Code the backward elimination. Entitle the section with the line «Way back:». Describe the steps as in the previous exercise (see the examples).
Accomplish the diagonal normalization. Entitle the section with the line «Diagonal normalization:»
The final answer should be also placed within the new section «result:»

 */

void inverse_matrix(){
    int n;
    cin >> n;
    squaredMatrix matrixA(n);
    cin >> matrixA;
    squaredMatrix tempMatrix(n);
    IdentityMatrix *identityMatrix = new IdentityMatrix(n);
    squaredMatrix square = *identityMatrix;
    cout << "step #0: Augmented Matrix\n";
    for (int i = 0; i < n; i++){
        for (int j = 0; j < 2 * n; j++){
            if (j < n){
                cout.precision(2);
                cout << fixed <<  matrixA.getElement(i, j) << " ";
            } else {
                cout << fixed << identityMatrix->getElement(i, j%n) << " ";
            }

        }
        cout << endl;
    }
    cout << "Direct way:\n";
    int step = 0;
    for (int k = 0; k < n - 1; k++) {
        int f = matrixA.findMaxColumn(k);
        if (f != k) {
            step++;
            cout << "step #" << step << ": permutation" << endl;
            PermutationMatrix *permutationMatrix = new PermutationMatrix(matrixA, k, f);
            square = *permutationMatrix * square;
            matrixA = *permutationMatrix * matrixA;
            for (int i = 0; i < n; i++){
                for (int j = 0; j < 2 * n; j++){
                    if (j < n){
                        cout.precision(2);
                        cout << fixed <<  matrixA.getElement(i, j) << " ";
                    } else {
                        cout << fixed << square.getElement(i, j%n) << " ";
                    }

                }
                cout << endl;
            }

        }
        bool flag = false;
        for (int i = k + 1; i < n; i++){
            if (fabs(matrixA.getElement(i, k)) >= limit){
                flag = true;
                break;
            }
        }
        if (flag) {
            for (int i = 0; i < n - k - 1; i++) {
                step++;
                cout << "step #" << step << ": elimination" << endl;
                EliminationMatrix *eliminationMatrix;
                eliminationMatrix = new EliminationMatrix(matrixA, k + i + 1, k);
                matrixA = *eliminationMatrix * matrixA;
                square = *eliminationMatrix * square;
                for (int i = 0; i < n; i++){
                    for (int j = 0; j < 2 * n; j++){
                        if (j < n){
                            cout.precision(2);
                            cout << fixed <<  matrixA.getElement(i, j) << " ";
                        } else {
                            cout << fixed << square.getElement(i, j%n) << " ";
                        }

                    }
                    cout << endl;
                }
            }
        }
    }
    cout << "Way back:\n";
    for (int k = n - 1; k > 0; k--){
        for (int i = k - 1; i >= 0; i--){
            step++;
            cout << "step #" << step << ": elimination" << endl;
            EliminationMatrix *eliminationMatrix;
            eliminationMatrix = new EliminationMatrix(matrixA, i, k);
            matrixA = *eliminationMatrix * matrixA;
            square = *eliminationMatrix * square;
            for (int i = 0; i < n; i++){
                for (int j = 0; j < 2 * n; j++){
                    if (j < n){
                        cout.precision(2);
                        cout << fixed <<  matrixA.getElement(i, j) << " ";
                    } else {
                        cout << fixed << square.getElement(i, j%n) << " ";
                    }

                }
                cout << endl;
            }
        }
    }
    cout << "Diagonal normalization:\n";
    for (int i = 0; i < n; i++){
        for (int j = 0; j < n; j++){
            double delimiter = matrixA.getElement(i, i);
            square.setElement(i,j,square.getElement(i,j)/delimiter);
        }
        matrixA.setElement(i,i, 1.00);
    }
    for (int i = 0; i < n; i++){
        for (int j = 0; j < 2 * n; j++){
            if (j < n){
                cout.precision(2);
                cout << fixed <<  matrixA.getElement(i, j) << " ";
            } else {
                cout << fixed << square.getElement(i, j%n) << " ";
            }

        }
        cout << endl;
    }
    cout << "result:\n";
    square.printMatrix();
}

/*
 * Input format
The input contains:

A square matrix A as in the previous exercise.
A vector of free coefficients b (in element-wise manner with the dimension firstly).
 * Output format
Code the elimination. Describe the steps as in the previous exercise (see the examples). Do not forget to print out a free vector. (No need to print direct way or way back more.)
Accomplish the diagonal normalization. Entitle the section with the line «Diagonal normalization:». Do not forget to print out a free vector.
The final answer should be also placed within the new section «result:»
 */

void linear_systems(){
    string s;
    int n;
    cin >> s;
    n = stoi(s);
    squaredMatrix matrixA(n);
    cin >> matrixA;
    cin >> s;
    n = stoi(s);
    ColumnVector columnVector(n);
    cin >> columnVector;
    cout << "step #0:\n";
    matrixA.printMatrix();
    columnVector.printColumnVector();
    int step = 0;
    for (int k = 0; k < n - 1; k++) {
        int f = matrixA.findMaxColumn(k);
        if (f != k) {
            step++;
            cout << "step #" << step << ": permutation" << endl;
            PermutationMatrix *permutationMatrix = new PermutationMatrix(matrixA, k, f);
            matrixA = *permutationMatrix * matrixA;
            columnVector = columnVector * *permutationMatrix;
            columnVector.printColumnVector();
        }
        bool flag = false;
        for (int i = k + 1; i < n; i++){
            if (fabs(matrixA.getElement(i, k)) >= limit){
                flag = true;
                break;
            }
        }
        if (flag) {
            for (int i = 0; i < n - k - 1; i++) {
                if (fabs(matrixA.getElement(k + i + 1, k)) >= limit) {
                    step++;
                    cout << "step #" << step << ": elimination" << endl;
                    EliminationMatrix *eliminationMatrix;
                    eliminationMatrix = new EliminationMatrix(matrixA, k + i + 1, k);
                    matrixA = *eliminationMatrix * matrixA;
                    columnVector = columnVector * *eliminationMatrix;
                    columnVector.printColumnVector();
                }
            }
        }
    }
    for (int k = n - 1; k > 0; k--){
        for (int i = k - 1; i >= 0; i--){
            if (fabs(matrixA.getElement(i, k)) >= limit) {
                step++;
                cout << "step #" << step << ": elimination" << endl;
                EliminationMatrix *eliminationMatrix;
                eliminationMatrix = new EliminationMatrix(matrixA, i, k);
                matrixA = *eliminationMatrix * matrixA;
                columnVector = columnVector * *eliminationMatrix;
                columnVector.printColumnVector();
            }
        }
    }
    cout << "Diagonal normalization:\n";
    columnVector.normalization(matrixA);
    matrixA.printMatrix();
    columnVector.printColumnVector();
    cout << "result:\n";
    columnVector.printColumnVector();
}

int main() {
    return 0;
}
