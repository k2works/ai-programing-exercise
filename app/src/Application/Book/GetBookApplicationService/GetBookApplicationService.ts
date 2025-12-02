import { injectable, inject } from 'tsyringe';
import { BookDTO } from '../BookDTO';
import { BookId } from 'Domain/models/Book/BookId/BookId';
import { IBookRepository } from 'Domain/models/Book/IBookRepository';

@injectable()
export class GetBookApplicationService {
  constructor(
    @inject('IBookRepository')
    private bookRepository: IBookRepository
  ) {}

  async execute(isbn: string): Promise<BookDTO | null> {
    const book = await this.bookRepository.find(new BookId(isbn));

    return book ? new BookDTO(book) : null;
  }
}
