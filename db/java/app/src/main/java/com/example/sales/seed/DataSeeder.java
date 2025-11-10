package com.example.sales.seed;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.example.sales.domain.model.Company;
import com.example.sales.domain.model.CompanyGroup;
import com.example.sales.domain.model.Customer;
import com.example.sales.domain.model.Department;
import com.example.sales.domain.model.Employee;
import com.example.sales.domain.model.Product;
import com.example.sales.domain.model.ProductCategory;
import com.example.sales.domain.model.Supplier;
import com.example.sales.domain.model.Warehouse;
import com.example.sales.domain.repository.CompanyGroupMapper;
import com.example.sales.domain.repository.CompanyMapper;
import com.example.sales.domain.repository.CustomerMapper;
import com.example.sales.domain.repository.DepartmentMapper;
import com.example.sales.domain.repository.EmployeeMapper;
import com.example.sales.domain.repository.ProductCategoryMapper;
import com.example.sales.domain.repository.ProductMapper;
import com.example.sales.domain.repository.SupplierMapper;
import com.example.sales.domain.repository.WarehouseMapper;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
@Profile("dev")
@RequiredArgsConstructor
public class DataSeeder implements ApplicationRunner {

    private final DepartmentMapper departmentMapper;
    private final EmployeeMapper employeeMapper;
    private final CompanyGroupMapper companyGroupMapper;
    private final CompanyMapper companyMapper;
    private final CustomerMapper customerMapper;
    private final SupplierMapper supplierMapper;
    private final ProductCategoryMapper productCategoryMapper;
    private final ProductMapper productMapper;
    private final WarehouseMapper warehouseMapper;

    @Override
    @Transactional
    public void run(ApplicationArguments args) {
        seedDepartments();
        seedEmployees();
        seedCompanyGroups();
        seedCompanies();
        seedCustomers();
        seedSuppliers();
        seedProductCategories();
        seedProducts();
        seedWarehouses();
    }

    private void seedDepartments() {
        LocalDate now = LocalDate.now();
        List<Department> departments = new ArrayList<>();
        departments.add(createDepartment("000000", now, "本社", "/000000", 1));
        addMeatDivisionDepartments(departments, now);
        addProcessedMeatDivisionDepartments(departments, now);
        addConsultingDivisionDepartments(departments, now);
        departments.forEach(departmentMapper::insert);
    }

    private void addMeatDivisionDepartments(List<Department> departments, LocalDate now) {
        departments.add(createDepartment("100000", now, "食肉製造・販売事業", "/000000/100000", 2));
        departments.add(createDepartment("110000", now, "食肉加工部門", "/000000/100000/110000", 3));
        departments.add(createDepartment("111000", now, "牛肉・豚肉・鶏肉課", "/000000/100000/110000/111000", 4));
        departments.add(createDepartment("112000", now, "食肉加工品課", "/000000/100000/110000/112000", 4));
        departments.add(createDepartment("120000", now, "小売販売部門", "/000000/100000/120000", 3));
        departments.add(createDepartment("121000", now, "直営小売店課", "/000000/100000/120000/121000", 4));
        departments.add(createDepartment("122000", now, "百貨店・スーパー向け販売課", "/000000/100000/120000/122000", 4));
        departments.add(createDepartment("130000", now, "新規取引先開拓部門", "/000000/100000/130000", 3));
        departments.add(createDepartment("131000", now, "ホテル・旅館向け課", "/000000/100000/130000/131000", 4));
        departments.add(createDepartment("132000", now, "飲食店向け課", "/000000/100000/130000/132000", 4));
    }

    private void addProcessedMeatDivisionDepartments(List<Department> departments, LocalDate now) {
        departments.add(createDepartment("200000", now, "食肉加工品事業", "/000000/200000", 2));
        departments.add(createDepartment("210000", now, "自社ブランド部門", "/000000/200000/210000", 3));
        departments.add(createDepartment("211000", now, "贈答用製品製造課", "/000000/200000/210000/211000", 4));
        departments.add(createDepartment("212000", now, "道の駅・土産物製品販売課", "/000000/200000/210000/212000", 4));
        departments.add(createDepartment("220000", now, "相手先ブランド製造(OEM)部門", "/000000/200000/220000", 3));
        departments.add(createDepartment("221000", now, "客先要望対応課", "/000000/200000/220000/221000", 4));
    }

    private void addConsultingDivisionDepartments(List<Department> departments, LocalDate now) {
        departments.add(createDepartment("300000", now, "コンサルティング事業", "/000000/300000", 2));
        departments.add(createDepartment("310000", now, "顧客対応部門", "/000000/300000/310000", 3));
        departments.add(createDepartment("311000", now, "メニュー提案課", "/000000/300000/310000/311000", 4));
        departments.add(createDepartment("312000", now, "半加工商品提供課", "/000000/300000/310000/312000", 4));
    }

    private Department createDepartment(String code, LocalDate startDate, String name, String path, int level) {
        Department dept = new Department();
        dept.setDepartmentCode(code);
        dept.setStartDate(startDate);
        dept.setDepartmentName(name);
        dept.setOrganizationLevel(level);
        dept.setDepartmentPath(path);
        dept.setLowestLevelFlag(1);
        dept.setSlipInputFlag(1);
        setAuditFields(dept, LocalDateTime.now());
        return dept;
    }

    private void seedEmployees() {
        List<Employee> employees = new ArrayList<>();
        seedManagementEmployees(employees);
        seedMeatDivisionEmployees(employees);
        seedProcessedMeatDivisionEmployees(employees);
        seedConsultingDivisionEmployees(employees);
        seedAdministrativeEmployees(employees);
        employees.forEach(employeeMapper::insert);
    }

    private void seedManagementEmployees(List<Employee> employees) {
        employees.add(createEmployee("EMP00001", "山田太郎", "ヤマダタロウ", "000000", "M", LocalDate.of(1965, 4, 1)));
        employees.add(createEmployee("EMP00002", "佐藤次郎", "サトウジロウ", "000000", "M", LocalDate.of(1970, 6, 15)));
    }

    private void seedMeatDivisionEmployees(List<Employee> employees) {
        employees.add(createEmployee("EMP00003", "鈴木三郎", "スズキサブロウ", "111000", "M", LocalDate.of(1985, 8, 20)));
        employees.add(createEmployee("EMP00004", "田中花子", "タナカハナコ", "111000", "F", LocalDate.of(1990, 3, 10)));
        employees.add(createEmployee("EMP00005", "高橋健一", "タカハシケンイチ", "111000", "M", LocalDate.of(1988, 11, 5)));
        employees.add(createEmployee("EMP00006", "伊藤美咲", "イトウミサキ", "111000", "F", LocalDate.of(1995, 7, 15)));
        employees.add(createEmployee("EMP00007", "渡辺大輔", "ワタナベダイスケ", "111000", "M", LocalDate.of(1992, 2, 28)));
        employees.add(createEmployee("EMP00008", "山本裕子", "ヤマモトユウコ", "112000", "F", LocalDate.of(1987, 5, 12)));
        employees.add(createEmployee("EMP00009", "中村正樹", "ナカムラマサキ", "112000", "M", LocalDate.of(1991, 9, 3)));
        employees.add(createEmployee("EMP00010", "小林愛", "コバヤシアイ", "112000", "F", LocalDate.of(1994, 1, 20)));
        employees.add(createEmployee("EMP00011", "加藤恵", "カトウメグミ", "121000", "F", LocalDate.of(1989, 12, 8)));
        employees.add(createEmployee("EMP00012", "吉田剛", "ヨシダタケシ", "121000", "M", LocalDate.of(1993, 6, 25)));
        employees.add(createEmployee("EMP00013", "清水陽子", "シミズヨウコ", "121000", "F", LocalDate.of(1996, 4, 14)));
        employees.add(createEmployee("EMP00014", "松本和彦", "マツモトカズヒコ", "121000", "M", LocalDate.of(1997, 10, 30)));
        employees.add(createEmployee("EMP00015", "森優子", "モリユウコ", "122000", "F", LocalDate.of(1986, 7, 18)));
        employees.add(createEmployee("EMP00016", "池田誠", "イケダマコト", "122000", "M", LocalDate.of(1990, 11, 22)));
        employees.add(createEmployee("EMP00017", "斉藤美穂", "サイトウミホ", "122000", "F", LocalDate.of(1992, 3, 5)));
    }

    private void seedProcessedMeatDivisionEmployees(List<Employee> employees) {
        employees.add(createEmployee("EMP00018", "前田浩二", "マエダコウジ", "211000", "M", LocalDate.of(1988, 2, 14)));
        employees.add(createEmployee("EMP00019", "岡田真理", "オカダマリ", "211000", "F", LocalDate.of(1991, 8, 27)));
        employees.add(createEmployee("EMP00020", "長谷川純", "ハセガワジュン", "211000", "M", LocalDate.of(1995, 5, 9)));
        employees.add(createEmployee("EMP00021", "藤原麻衣", "フジワラマイ", "211000", "F", LocalDate.of(1994, 12, 16)));
        employees.add(createEmployee("EMP00022", "村上拓也", "ムラカミタクヤ", "211000", "M", LocalDate.of(1996, 9, 21)));
        employees.add(createEmployee("EMP00023", "後藤綾", "ゴトウアヤ", "212000", "F", LocalDate.of(1989, 6, 7)));
        employees.add(createEmployee("EMP00024", "青木大地", "アオキダイチ", "212000", "M", LocalDate.of(1992, 10, 13)));
        employees.add(createEmployee("EMP00025", "石井里奈", "イシイリナ", "212000", "F", LocalDate.of(1995, 3, 28)));
        employees.add(createEmployee("EMP00026", "山口翔太", "ヤマグチショウタ", "212000", "M", LocalDate.of(1997, 7, 4)));
        employees.add(createEmployee("EMP00027", "木村彩香", "キムラアヤカ", "212000", "F", LocalDate.of(1998, 1, 19)));
        employees.add(createEmployee("EMP00028", "林修", "ハヤシオサム", "221000", "M", LocalDate.of(1987, 4, 23)));
        employees.add(createEmployee("EMP00029", "原田香織", "ハラダカオリ", "221000", "F", LocalDate.of(1990, 8, 11)));
        employees.add(createEmployee("EMP00030", "坂本勇太", "サカモトユウタ", "221000", "M", LocalDate.of(1993, 2, 6)));
        employees.add(createEmployee("EMP00031", "内田美和", "ウチダミワ", "221000", "F", LocalDate.of(1996, 11, 25)));
    }

    private void seedConsultingDivisionEmployees(List<Employee> employees) {
        employees.add(createEmployee("EMP00032", "竹内洋介", "タケウチヨウスケ", "311000", "M", LocalDate.of(1986, 5, 17)));
        employees.add(createEmployee("EMP00033", "橋本奈々", "ハシモトナナ", "311000", "F", LocalDate.of(1989, 9, 2)));
        employees.add(createEmployee("EMP00034", "上田剛志", "ウエダタケシ", "311000", "M", LocalDate.of(1991, 12, 29)));
        employees.add(createEmployee("EMP00035", "西村沙織", "ニシムラサオリ", "311000", "F", LocalDate.of(1994, 6, 8)));
        employees.add(createEmployee("EMP00036", "中島健太", "ナカジマケンタ", "311000", "M", LocalDate.of(1997, 3, 15)));
        employees.add(createEmployee("EMP00037", "大野友美", "オオノトモミ", "311000", "F", LocalDate.of(1998, 10, 22)));
        employees.add(createEmployee("EMP00038", "小川智也", "オガワトモヤ", "312000", "M", LocalDate.of(1988, 1, 26)));
        employees.add(createEmployee("EMP00039", "平野麻美", "ヒラノアサミ", "312000", "F", LocalDate.of(1990, 7, 13)));
        employees.add(createEmployee("EMP00040", "井上隆", "イノウエタカシ", "312000", "M", LocalDate.of(1993, 4, 9)));
        employees.add(createEmployee("EMP00041", "野口真由", "ノグチマユ", "312000", "F", LocalDate.of(1995, 11, 3)));
        employees.add(createEmployee("EMP00042", "三浦翔", "ミウラショウ", "312000", "M", LocalDate.of(1998, 2, 18)));
        employees.add(createEmployee("EMP00043", "杉山亜美", "スギヤマアミ", "312000", "F", LocalDate.of(1999, 8, 7)));
    }

    private void seedAdministrativeEmployees(List<Employee> employees) {
        employees.add(createEmployee("EMP00044", "佐々木聡", "ササキサトシ", "000000", "M", LocalDate.of(1984, 10, 5)));
        employees.add(createEmployee("EMP00045", "松田由美", "マツダユミ", "000000", "F", LocalDate.of(1987, 3, 12)));
    }

    private Employee createEmployee(String code, String name, String kanaName, String deptCode,
                                   String gender, LocalDate birthDate) {
        Employee emp = new Employee();
        emp.setEmployeeCode(code);
        emp.setEmployeeName(name);
        emp.setEmployeeNameKana(kanaName);
        emp.setGender(gender);
        emp.setBirthDate(birthDate);
        emp.setJoinDate(LocalDate.of(2020, 4, 1));
        emp.setDepartmentCode(deptCode);
        emp.setPositionCode("01");
        setAuditFields(emp, LocalDateTime.now());
        return emp;
    }

    private void seedCompanyGroups() {
        LocalDateTime now = LocalDateTime.now();
        List<CompanyGroup> groups = new ArrayList<>();
        groups.add(createCompanyGroup("GRP001", "百貨店", now));
        groups.add(createCompanyGroup("GRP002", "スーパー", now));
        groups.add(createCompanyGroup("GRP003", "ホテル・旅館", now));
        groups.add(createCompanyGroup("GRP004", "飲食店", now));
        groups.add(createCompanyGroup("GRP005", "観光施設", now));
        groups.add(createCompanyGroup("GRP006", "食肉卸", now));
        groups.add(createCompanyGroup("GRP007", "畜産業者", now));
        groups.forEach(companyGroupMapper::insert);
    }

    private CompanyGroup createCompanyGroup(String code, String name, LocalDateTime timestamp) {
        CompanyGroup group = new CompanyGroup();
        group.setCompanyGroupCode(code);
        group.setCompanyGroupName(name);
        setAuditFields(group, timestamp);
        return group;
    }

    private void seedCompanies() {
        LocalDateTime now = LocalDateTime.now();
        List<Company> companies = new ArrayList<>();
        addCustomerCompanies(companies, now);
        addSupplierCompanies(companies, now);
        companies.forEach(companyMapper::insert);
    }

    private void addCustomerCompanies(List<Company> companies, LocalDateTime now) {
        companies.add(createCompany("CMP001", "地域百貨店", "チイキヒャッカテン", "GRP001", 1, now));
        companies.add(createCompany("CMP002", "X県有名百貨店", "Xケンユウメイヒャッカテン", "GRP001", 1, now));
        companies.add(createCompany("CMP003", "地域スーパーチェーン", "チイキスーパーチェーン", "GRP002", 1, now));
        companies.add(createCompany("CMP004", "広域スーパーチェーン", "コウイキスーパーチェーン", "GRP002", 1, now));
        companies.add(createCompany("CMP005", "シティホテルX", "シティホテルX", "GRP003", 1, now));
        companies.add(createCompany("CMP006", "温泉旅館Y", "オンセンリョカンY", "GRP003", 1, now));
        companies.add(createCompany("CMP007", "焼肉レストランA", "ヤキニクレストランA", "GRP004", 1, now));
        companies.add(createCompany("CMP008", "イタリアンレストランB", "イタリアンレストランB", "GRP004", 1, now));
        companies.add(createCompany("CMP009", "道の駅C", "ミチノエキC", "GRP005", 1, now));
        companies.add(createCompany("CMP010", "観光センターD", "カンコウセンターD", "GRP005", 1, now));
    }

    private void addSupplierCompanies(List<Company> companies, LocalDateTime now) {
        companies.add(createCompany("CMP011", "地域食肉卸A社", "チイキショクニクオロシAシャ", "GRP006", 2, now));
        companies.add(createCompany("CMP012", "地域食肉卸B社", "チイキショクニクオロシBシャ", "GRP006", 2, now));
        companies.add(createCompany("CMP013", "地域畜産農家", "チイキチクサンノウカ", "GRP007", 2, now));
        companies.add(createCompany("CMP014", "県内畜産組合", "ケンナイチクサンクミアイ", "GRP007", 2, now));
    }

    private Company createCompany(String code, String name, String kanaName, String groupCode,
                                 int supplierType, LocalDateTime timestamp) {
        Company company = new Company();
        company.setCompanyCode(code);
        company.setCompanyName(name);
        company.setCompanyNameKana(kanaName);
        company.setSupplierType(supplierType);
        company.setZipCode("000-0000");
        company.setState("X県");
        company.setAddress1("X市Y町1-2-3");
        company.setAddress2("");
        company.setNoSalesFlag(0);
        company.setWideUseType(1);
        company.setCompanyGroupCode(groupCode);
        company.setMaxCredit(10000000);
        company.setTempCreditUp(0);
        setAuditFields(company, timestamp);
        return company;
    }

    private void seedCustomers() {
        LocalDateTime now = LocalDateTime.now();
        List<Customer> customers = new ArrayList<>();
        for (int i = 1; i <= 10; i++) {
            customers.add(createCustomer(String.format("CMP%03d", i), 0, now));
        }
        customers.forEach(customerMapper::insert);
    }

    private Customer createCustomer(String customerCode, int branch, LocalDateTime timestamp) {
        Customer customer = new Customer();
        customer.setCustomerCode(customerCode);
        customer.setCustomerBranch(branch);
        customer.setCustomerType(1);
        customer.setArCode(customerCode);
        customer.setArBranch(branch);
        customer.setPayerCode(customerCode);
        customer.setPayerBranch(branch);
        customer.setCustomerName("顧客_" + customerCode);
        customer.setCustomerNameKana("コキャク_" + customerCode);
        customer.setEmployeeCode("EMP00001");
        customer.setCustomerUserName("担当者");
        customer.setCustomerDepartmentName("営業部");
        setCommonAddress(customer);
        customer.setCustomerArType(1);
        setPaymentTerms(customer);
        setAuditFields(customer, timestamp);
        return customer;
    }

    private void setCommonAddress(Customer customer) {
        customer.setCustomerZipCode("000-0000");
        customer.setCustomerState("X県");
        customer.setCustomerAddress1("X市Y町1-2-3");
        customer.setCustomerAddress2("");
        customer.setCustomerTel("000-0000-0000");
        customer.setCustomerFax("000-0000-0001");
        customer.setCustomerEmail("contact@example.com");
    }

    private void setPaymentTerms(Customer customer) {
        customer.setCustomerCloseDate1(31);
        customer.setCustomerPayMonths1(1);
        customer.setCustomerPayDates1(31);
        customer.setCustomerPayMethod1(1);
        customer.setCustomerCloseDate2(0);
        customer.setCustomerPayMonths2(0);
        customer.setCustomerPayDates2(0);
        customer.setCustomerPayMethod2(0);
    }

    private void setAuditFields(Customer c, LocalDateTime t) {
        c.setCreatedAt(t); c.setCreatedBy("SYSTEM"); c.setUpdatedAt(t); c.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(Supplier s, LocalDateTime t) {
        s.setCreatedAt(t); s.setCreatedBy("SYSTEM"); s.setUpdatedAt(t); s.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(CompanyGroup cg, LocalDateTime t) {
        cg.setCreatedAt(t); cg.setCreatedBy("SYSTEM"); cg.setUpdatedAt(t); cg.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(Company c, LocalDateTime t) {
        c.setCreatedAt(t); c.setCreatedBy("SYSTEM"); c.setUpdatedAt(t); c.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(ProductCategory pc, LocalDateTime t) {
        pc.setCreatedAt(t); pc.setCreatedBy("SYSTEM"); pc.setUpdatedAt(t); pc.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(Product p, LocalDateTime t) {
        p.setCreatedAt(t); p.setCreatedBy("SYSTEM"); p.setUpdatedAt(t); p.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(Warehouse w, LocalDateTime t) {
        w.setCreatedAt(t); w.setCreatedBy("SYSTEM"); w.setUpdatedAt(t); w.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(Department d, LocalDateTime t) {
        d.setCreatedAt(t); d.setCreatedBy("SYSTEM"); d.setUpdatedAt(t); d.setUpdatedBy("SYSTEM");
    }
    private void setAuditFields(Employee e, LocalDateTime t) {
        e.setCreatedAt(t); e.setCreatedBy("SYSTEM"); e.setUpdatedAt(t); e.setUpdatedBy("SYSTEM");
    }

    private void seedSuppliers() {
        LocalDateTime now = LocalDateTime.now();
        List<Supplier> suppliers = new ArrayList<>();
        for (int i = 11; i <= 14; i++) {
            suppliers.add(createSupplier(String.format("CMP%03d", i), 0, now));
        }
        suppliers.forEach(supplierMapper::insert);
    }

    private Supplier createSupplier(String supplierCode, int branch, LocalDateTime timestamp) {
        Supplier supplier = new Supplier();
        supplier.setSupplierCode(supplierCode);
        supplier.setSupplierBranch(branch);
        supplier.setSupplierName("仕入先_" + supplierCode);
        supplier.setSupplierNameKana("シイレサキ_" + supplierCode);
        supplier.setSupplierUserName("担当者");
        supplier.setSupplierDepartmentName("営業部");
        supplier.setSupplierZipCode("000-0000");
        supplier.setSupplierState("X県");
        supplier.setSupplierAddress1("X市Y町1-2-3");
        supplier.setSupplierAddress2("");
        supplier.setSupplierTel("000-0000-0000");
        supplier.setSupplierFax("000-0000-0001");
        supplier.setSupplierEmail("contact@example.com");
        supplier.setSupplierCloseDate(31);
        supplier.setSupplierPayMonths(1);
        supplier.setSupplierPayDates(31);
        supplier.setSupplierPayMethod(1);
        setAuditFields(supplier, timestamp);
        return supplier;
    }

    private void seedProductCategories() {
        LocalDateTime now = LocalDateTime.now();
        List<ProductCategory> categories = new ArrayList<>();
        categories.add(createProductCategory("CAT001", "牛肉", 1, "/CAT001", now));
        categories.add(createProductCategory("CAT002", "豚肉", 1, "/CAT002", now));
        categories.add(createProductCategory("CAT003", "鶏肉", 1, "/CAT003", now));
        categories.add(createProductCategory("CAT004", "加工品", 1, "/CAT004", now));
        categories.forEach(productCategoryMapper::insert);
    }

    private ProductCategory createProductCategory(String code, String name, int level, String path,
                                                 LocalDateTime timestamp) {
        ProductCategory category = new ProductCategory();
        category.setProductCategoryCode(code);
        category.setProductCategoryName(name);
        category.setProductCategoryLevel(level);
        category.setProductCategoryPath(path);
        category.setLowestLevelFlag(1);
        setAuditFields(category, timestamp);
        return category;
    }

    private void seedProducts() {
        LocalDateTime now = LocalDateTime.now();
        List<Product> products = new ArrayList<>();
        addBeefProducts(products, now);
        addPorkProducts(products, now);
        addChickenProducts(products, now);
        addProcessedProducts(products, now);
        products.forEach(productMapper::insert);
    }

    private void addBeefProducts(List<Product> products, LocalDateTime now) {
        products.add(createProduct("PRD001", "黒毛和牛サーロイン", "CAT001", 15000, 10000, "CMP011", now));
        products.add(createProduct("PRD002", "黒毛和牛ロース", "CAT001", 12000, 8000, "CMP011", now));
        products.add(createProduct("PRD003", "黒毛和牛カルビ", "CAT001", 10000, 7000, "CMP011", now));
        products.add(createProduct("PRD004", "黒毛和牛ヒレ", "CAT001", 18000, 12000, "CMP012", now));
        products.add(createProduct("PRD005", "黒毛和牛切り落とし", "CAT001", 5000, 3500, "CMP012", now));
    }

    private void addPorkProducts(List<Product> products, LocalDateTime now) {
        products.add(createProduct("PRD006", "豚ロース", "CAT002", 3000, 2000, "CMP011", now));
        products.add(createProduct("PRD007", "豚バラ", "CAT002", 2500, 1700, "CMP011", now));
        products.add(createProduct("PRD008", "豚ヒレ", "CAT002", 4000, 2800, "CMP012", now));
        products.add(createProduct("PRD009", "豚コマ", "CAT002", 1500, 1000, "CMP012", now));
        products.add(createProduct("PRD010", "豚肩ロース", "CAT002", 3500, 2400, "CMP012", now));
    }

    private void addChickenProducts(List<Product> products, LocalDateTime now) {
        products.add(createProduct("PRD011", "鶏もも", "CAT003", 1500, 1000, "CMP013", now));
        products.add(createProduct("PRD012", "鶏むね", "CAT003", 1000, 700, "CMP013", now));
        products.add(createProduct("PRD013", "手羽先", "CAT003", 1200, 800, "CMP013", now));
        products.add(createProduct("PRD014", "手羽元", "CAT003", 1100, 750, "CMP014", now));
        products.add(createProduct("PRD015", "鶏ささみ", "CAT003", 1300, 900, "CMP014", now));
    }

    private void addProcessedProducts(List<Product> products, LocalDateTime now) {
        products.add(createProduct("PRD016", "ローストビーフ", "CAT004", 8000, 5500, "CMP011", now));
        products.add(createProduct("PRD017", "ハム", "CAT004", 3000, 2000, "CMP011", now));
        products.add(createProduct("PRD018", "ソーセージ", "CAT004", 2500, 1700, "CMP012", now));
        products.add(createProduct("PRD019", "ベーコン", "CAT004", 3500, 2400, "CMP012", now));
        products.add(createProduct("PRD020", "コロッケ", "CAT004", 500, 300, "CMP013", now));
    }

    private Product createProduct(String code, String name, String categoryCode, int sellingPrice,
                                 int purchasePrice, String supplierCode, LocalDateTime timestamp) {
        Product product = new Product();
        product.setProductCode(code);
        product.setProductFormalName(name);
        product.setProductAbbreviation(name);
        product.setProductNameKana(name);
        product.setProductType("1");
        product.setModelNumber("");
        product.setSellingPrice(sellingPrice);
        product.setPurchasePrice(purchasePrice);
        product.setCostOfSales(purchasePrice);
        product.setTaxType(1);
        product.setProductCategoryCode(categoryCode);
        product.setMiscellaneousType(0);
        product.setInventoryManagementFlag(1);
        product.setInventoryAllocationFlag(1);
        product.setSupplierCode(supplierCode);
        product.setSupplierBranch(0);
        setAuditFields(product, timestamp);
        return product;
    }

    private void seedWarehouses() {
        LocalDateTime now = LocalDateTime.now();
        List<Warehouse> warehouses = new ArrayList<>();
        warehouses.add(createWarehouse("WH001", "本社倉庫", 1, "X県X市Y町1-2-3", "000-0000-0000", "EMP00001", now));
        warehouses.add(createWarehouse("WH002", "工場倉庫", 2, "X県X市Z町4-5-6", "000-0000-0001", "EMP00002", now));
        warehouses.forEach(warehouseMapper::insert);
    }

    private Warehouse createWarehouse(String code, String name, int type, String address,
                                     String phoneNumber, String managerCode, LocalDateTime timestamp) {
        Warehouse warehouse = new Warehouse();
        warehouse.setWarehouseCode(code);
        warehouse.setWarehouseName(name);
        warehouse.setWarehouseType(type);
        warehouse.setAddress(address);
        warehouse.setPhoneNumber(phoneNumber);
        warehouse.setManagerCode(managerCode);
        setAuditFields(warehouse, timestamp);
        return warehouse;
    }
}
