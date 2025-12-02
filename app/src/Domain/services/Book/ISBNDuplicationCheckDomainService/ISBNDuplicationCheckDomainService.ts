import { BookId } from 'Domain/models/Book/BookId/BookId';

export class ISBNDuplicationCheckDomainService {
  async execute(_isbn: BookId): Promise<boolean> {
    const isDuplicateISBN = false;

    return isDuplicateISBN;
  }
}
